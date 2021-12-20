import day18.Utils.printAny

def binaryToDecimal(value: String) = Integer.parseInt(value, 2)

//binaryToDecimal("101010001100011111110001001011101001")

BigInt("101010001100011111110001001011101001", 2).longValue

//"101010001100011111110001001011101001".toH

//20.toBinaryString


//"abc".toBoolean
//"trues".toBoolean

Integer.parseInt("0110", 2)
Integer.parseInt("0111", 2)
Integer.parseInt("0101", 2)

Integer.parseInt("0000", 2)
Integer.parseInt("1111", 2)



String.format("%4s", 6.toBinaryString).replace(' ', '0')
String.format("%4s", 16.toBinaryString).replace(' ', '0')


//_ <- printAny { number.get("0110") }
//_ <- printAny { number.get("0111") }
//_ <- printAny { number.get("010") } // 0101
// [[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]]
//_ <- printAny { number.get("0111") }
//_ <- printAny { number.get("0110") }
//_ <- printAny { number.get("1") } // 1000