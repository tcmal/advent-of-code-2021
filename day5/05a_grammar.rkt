#lang brag
vents: (line "\n")* ["\n"]
line: coord " " "-" ">" " " coord
coord: number "," number
number: digit*
digit: "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
