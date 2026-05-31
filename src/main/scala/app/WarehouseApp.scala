package app

import domain.*
import domain.WarehouseProgram.processOrder
import monads.IO

object WarehouseApp:
  private val keyboard = Product("kb", "Клавиатура", unitWeight = 0.8, unitPrice = BigDecimal(2500))
  private val mouse = Product("ms", "Мышь", unitWeight = 0.2, unitPrice = BigDecimal(1200))
  private val monitor = Product("mn", "Монитор", unitWeight = 4.5, unitPrice = BigDecimal(18000))

  private val config = WarehouseConfig(
    shippingRates = List(
      ShippingRate(1.0, 200),
      ShippingRate(5.0, 450),
      ShippingRate(15.0, 800)
    ),
    maxParcelWeight = 25.0,
    packagingRules = List(
      PackagingRule(1.0, "Малый пакет"),
      PackagingRule(5.0, "Средняя коробка"),
      PackagingRule(25.0, "Крупная упаковка")
    ),
    freeShippingFrom = BigDecimal(20000)
  )

  private val initialState = WarehouseState(
    inventory = Map(
      keyboard.id -> InventoryItem(keyboard, 20),
      mouse.id -> InventoryItem(mouse, 30),
      monitor.id -> InventoryItem(monitor, 10)
    ),
    packedOrders = Map.empty,
    shippedOrders = Map.empty
  )

  private def parseOrderItems(raw: String): Either[String, List[OrderItem]] =
    val parts = raw.split(",").toList.map(_.trim).filter(_.nonEmpty)
    val parsed = parts.map { token =>
      token.split(":").toList match
        case id :: qty :: Nil =>
          qty.toIntOption match
            case Some(q) if q > 0 => Right(OrderItem(id.trim, q))
            case _                => Left(s"Некорректное количество: $token")
        case _ => Left(s"Некорректный формат позиции: $token")
    }
    parsed.foldRight[Either[String, List[OrderItem]]](Right(Nil)) {
      case (Right(item), Right(acc)) => Right(item :: acc)
      case (Left(err), _)            => Left(err)
      case (_, Left(err))            => Left(err)
    }

  private val program: IO[Unit] =
    for
      _ <- IO.putStrLn("Введите id заказа:")
      orderId <- IO.readLine
      _ <- IO.putStrLn("Введите позиции в формате id:кол-во,id:кол-во (например kb:1,mn:1)")
      rawItems <- IO.readLine
      parsed <- IO.pure(parseOrderItems(rawItems))
      _ <- parsed match
        case Left(err) => IO.putStrLn(s"Ошибка ввода: $err")
        case Right(items) =>
          val order = Order(orderId, items)
          val (_, resultWithLog) = processOrder(order, initialState, config)
          val logOutput = resultWithLog.log.mkString("\n")
          val resultOutput = resultWithLog.value match
            case Left(err) =>
              s"Заказ не собран: $err"
            case Right(packed) =>
              s"""Заказ собран и отправлен.
                 |Упаковка: ${packed.packageType}
                 |Доставка: ${packed.shippingCost}
                 |Вес: ${packed.totalWeight}
                 |Сумма: ${packed.totalPrice}""".stripMargin
          IO.putStrLn(s"$logOutput\n$resultOutput")
    yield ()

  @main def runWarehouseApp(): Unit =
    program.unsafeRun()
