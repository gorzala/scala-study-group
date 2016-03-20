import ssg.chapter_one._

val myCard = CreditCard("Marc")
val myCafeBar = new Cafe

val myPurchase = myCafeBar.buyCoffee(myCard)

val myPurchases = myCafeBar.buyCoffees(myCard, 4)

val x = Question.today









val listOfTuples : List[(Char,Int)] = List(('a',1),('b',2),('c',3))

val (chars, numbers) = listOfTuples.unzip
val amexCC = CreditCard("amex")
val amexCharge1 = Charge(amexCC, 3.4)
val amexCharge2 = Charge(amexCC, 2.6)
val amexCharges = List(amexCharge1, amexCharge2)
val visaCC = CreditCard("visa")
val visaCharge1 = Charge(visaCC, 7)
val visaCharges = List(visaCharge1)
val totalCharges = visaCharges ::: amexCharges
val coalescedCharges = myCafeBar.coalesce(totalCharges)

