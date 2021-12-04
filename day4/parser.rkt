#lang brag
file: numbers "\n" "\n" boards
numbers: number ("," number)*
number: digit+
digit: "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
boards: board ("\n" board)*
board: row "\n" row "\n" row "\n" row "\n" row "\n"
row: " "+ number " "+ number " "+ number " "+ number " "+ number
