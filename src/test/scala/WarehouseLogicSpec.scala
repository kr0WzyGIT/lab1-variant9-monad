import domain.*
import domain.WarehouseProgram.processOrder
import org.scalatest.funsuite.AnyFunSuite

class WarehouseLogicSpec extends AnyFunSuite:
  private val keyboard = Product("kb", "Клавиатура", 0.8, BigDecimal(2500))
  private val monitor = Product("mn", "Монитор", 4.5, BigDecimal(18000))

  private val config = WarehouseConfig(
    shippingRates = List(
      ShippingRate(1.0, 200),
      ShippingRate(5.0, 450),
      ShippingRate(15.0, 800)
    ),
    maxParcelWeight = 30.0,
    packagingRules = List(
      PackagingRule(1.0, "Малый пакет"),
      PackagingRule(5.0, "Средняя коробка"),
      PackagingRule(30.0, "Крупная упаковка")
    ),
    freeShippingFrom = BigDecimal(20000)
  )

  private def state(kbQty: Int, monQty: Int) =
    WarehouseState(
      inventory = Map(
        keyboard.id -> InventoryItem(keyboard, kbQty),
        monitor.id -> InventoryItem(monitor, monQty)
      ),
      packedOrders = Map.empty,
      shippedOrders = Map.empty
    )

  test("Товара хватает -> заказ собирается"):
    val order = Order("ok-1", List(OrderItem("kb", 2)))
    val (_, result) = processOrder(order, state(10, 2), config)
    assert(result.value.isRight)

  test("Товара не хватает -> отказ"):
    val order = Order("fail-1", List(OrderItem("kb", 50)))
    val (_, result) = processOrder(order, state(10, 2), config)
    assert(result.value.isLeft)

  test("Тяжелый заказ -> выбирается крупная упаковка"):
    val order = Order("heavy-1", List(OrderItem("mn", 2)))
    val (_, result) = processOrder(order, state(2, 5), config)
    assert(result.value.exists(_.packageType == "Крупная упаковка"))

  test("Дорогой заказ -> доставка бесплатна"):
    val order = Order("expensive-1", List(OrderItem("mn", 2)))
    val (_, result) = processOrder(order, state(2, 5), config)
    assert(result.value.exists(_.shippingCost == BigDecimal(0)))
