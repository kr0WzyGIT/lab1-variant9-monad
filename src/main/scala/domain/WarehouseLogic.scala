package domain

import monads.{Reader, State, Writer}
import monads.LogMonoid.given

object WarehouseReader:
  def freeShipping(totalPrice: BigDecimal): Reader[WarehouseConfig, Boolean] =
    Reader(config => totalPrice >= config.freeShippingFrom)

  def packageType(weight: Double): Reader[WarehouseConfig, String] =
    Reader(config =>
      config.packagingRules
        .sortBy(_.maxWeight)
        .find(weight <= _.maxWeight)
        .map(_.packageName)
        .getOrElse("Крупная усиленная")
    )

  def shippingCost(weight: Double, totalPrice: BigDecimal): Reader[WarehouseConfig, BigDecimal] =
    for
      isFree <- freeShipping(totalPrice)
      cost <- Reader { config =>
        if isFree then BigDecimal(0)
        else
          config.shippingRates
            .sortBy(_.maxWeight)
            .find(weight <= _.maxWeight)
            .map(_.cost)
            .getOrElse(BigDecimal(900))
      }
    yield cost

  def canAssemble(order: Order, inventory: Map[String, InventoryItem]): Reader[WarehouseConfig, Boolean] =
    Reader { config =>
      val hasStock = order.items.forall { item =>
        inventory.get(item.productId).exists(_.quantity >= item.quantity)
      }
      val totalWeight = order.items.foldLeft(0.0) { (acc, item) =>
        val oneWeight = inventory.get(item.productId).map(_.product.unitWeight).getOrElse(0.0)
        acc + oneWeight * item.quantity
      }
      hasStock && totalWeight <= config.maxParcelWeight
    }

object WarehouseStateOps:
  type Log = Vector[String]

  def receiveShipment(items: List[(Product, Int)]): State[WarehouseState, Unit] =
    State.modify { st =>
      val updated = items.foldLeft(st.inventory) { case (inv, (product, delta)) =>
        val current = inv.get(product.id).map(_.quantity).getOrElse(0)
        inv.updated(product.id, InventoryItem(product, current + delta))
      }
      st.copy(inventory = updated)
    }

  def reserveItems(order: Order): State[WarehouseState, Either[String, Map[String, Int]]] =
    State { st =>
      val canReserve = order.items.forall(it => st.inventory.get(it.productId).exists(_.quantity >= it.quantity))
      if !canReserve then (st, Left("Недостаточно товара на складе"))
      else
        val updatedInventory = order.items.foldLeft(st.inventory) { (inv, item) =>
          val old = inv(item.productId)
          inv.updated(item.productId, old.copy(quantity = old.quantity - item.quantity))
        }
        val reservedMap = order.items.map(i => i.productId -> i.quantity).toMap
        (st.copy(inventory = updatedInventory), Right(reservedMap))
    }

  def packOrder(packed: PackedOrder): State[WarehouseState, Either[String, PackedOrder]] =
    State { st =>
      if st.packedOrders.contains(packed.orderId) then (st, Left("Заказ уже упакован"))
      else
        val next = st.copy(packedOrders = st.packedOrders.updated(packed.orderId, packed))
        (next, Right(packed))
    }

  def shipOrder(orderId: String): State[WarehouseState, Either[String, ShippedOrder]] =
    State { st =>
      st.packedOrders.get(orderId) match
        case None => (st, Left("Заказ не найден среди упакованных"))
        case Some(packed) =>
          val shipped = ShippedOrder(packed.orderId, packed.packageType, packed.shippingCost)
          val next = st.copy(
            packedOrders = st.packedOrders - orderId,
            shippedOrders = st.shippedOrders.updated(orderId, shipped)
          )
          (next, Right(shipped))
    }

object WarehouseProgram:
  import WarehouseReader._
  import WarehouseStateOps._

  type Log = Vector[String]

  private def calculateTotals(
      order: Order,
      inventory: Map[String, InventoryItem]
  ): Either[String, (Double, BigDecimal)] =
    order.items.foldLeft[Either[String, (Double, BigDecimal)]](Right((0.0, BigDecimal(0)))) {
      case (accEither, item) =>
        for
          acc <- accEither
          inv <- inventory.get(item.productId).toRight(s"Неизвестный товар: ${item.productId}")
          (w, p) = acc
        yield (
          w + inv.product.unitWeight * item.quantity,
          p + inv.product.unitPrice * item.quantity
        )
    }

  def processOrder(order: Order, state: WarehouseState, config: WarehouseConfig): (WarehouseState, Writer[Log, Either[String, PackedOrder]]) =
    val baseLog = Vector(s"Начинаем обработку заказа ${order.id}")

    val earlyValidation =
      for
        totals <- calculateTotals(order, state.inventory)
        (weight, totalPrice) = totals
        can = canAssemble(order, state.inventory).run(config)
        _ <- Either.cond(can, (), "Заказ не проходит проверку Reader (вес/остатки)")
      yield (weight, totalPrice)

    earlyValidation match
      case Left(err) =>
        (state, Writer(baseLog ++ Vector(s"Проверка не пройдена: $err"), Left(err)))
      case Right((weight, totalPrice)) =>
        val (stateAfterReserve, reserveResult) = reserveItems(order).run(state)
        reserveResult match
          case Left(err) =>
            (
              state,
              Writer(
                baseLog ++ Vector(s"Резервирование отклонено: $err"),
                Left(err)
              )
            )
          case Right(reserved) =>
            val pkg = packageType(weight).run(config)
            val delivery = shippingCost(weight, totalPrice).run(config)
            val packed = PackedOrder(order.id, reserved, weight, totalPrice, pkg, delivery)
            val (stateAfterPack, packResult) = packOrder(packed).run(stateAfterReserve)

            packResult match
              case Left(err) =>
                (
                  state,
                  Writer(
                    baseLog ++ Vector(
                      s"Резервирование успешно: $reserved",
                      s"Расчет веса: $weight",
                      s"Выбор упаковки: $pkg",
                      s"Стоимость доставки: $delivery",
                      s"Упаковка не выполнена: $err"
                    ),
                    Left(err)
                  )
                )
              case Right(donePacked) =>
                val (stateAfterShip, _) = shipOrder(donePacked.orderId).run(stateAfterPack)
                (
                  stateAfterShip,
                  Writer(
                    baseLog ++ Vector(
                      s"Резервирование успешно: $reserved",
                      s"Расчет веса: $weight",
                      s"Выбор упаковки: $pkg",
                      s"Стоимость доставки: $delivery"
                    ),
                    Right(donePacked)
                  )
                )
