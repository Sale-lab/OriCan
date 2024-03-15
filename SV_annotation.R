
########
# Simple SV annotation

library(VariantAnnotation)
library(StructuralVariantAnnotation)
library(stringr)

# Information
# insLen: Integer vector of same length as ‘transitiveGr‘ indicating the number of bases inserted at the breakpoint

# Simple SV type classifier funtion
simpleEventType <- function(gr) {
  return(ifelse(seqnames(gr) != seqnames(partner(gr)), "ITX",
                ifelse(gr$insLen >= abs(gr$svLen) * 0.7, "INS", 
                       ifelse(strand(gr) == strand(partner(gr)), "INV",
                              ifelse(xor(start(gr) < start(partner(gr)), strand(gr) == "-"), "DEL", "DUP")))))
}















