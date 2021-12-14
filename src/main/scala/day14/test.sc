val res: Long = 4384079139203L

  (res.toDouble / 2).toLong + res % 2

val s: String = "SD"

def countMatches(string: String, subset: String) = string.length() - string.replaceAll(subset,"").length()

countMatches("SS", "S")

