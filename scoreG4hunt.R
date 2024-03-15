
######################################################
# Core algorithm for G4Hunter
######################################################


###### G4translate change the DNA code into G4Hunter code.
###### Only G or C are taken into account. non G/C bases are translated in 0
###### It is OK if N or U are in the sequence, but might be a problem if other letters or numbers are present
G4translate=function(x)
	# x is a Rle of a sequence
{
	x <- toupper(x)
	xres=x
	runValue(xres)[runValue(x)=='C' & runLength(x)>3]=-4
	runValue(xres)[runValue(x)=='C' & runLength(x)==3]=-3
	runValue(xres)[runValue(x)=='C' & runLength(x)==2]=-2
	runValue(xres)[runValue(x)=='C' & runLength(x)==1]=-1
	runValue(xres)[runValue(x)=='G' & runLength(x)>3]=4
	runValue(xres)[runValue(x)=='G' & runLength(x)==3]=3
	runValue(xres)[runValue(x)=='G' & runLength(x)==2]=2
	runValue(xres)[runValue(x)=='G' & runLength(x)==1]=1
	runValue(xres)[runValue(x)!='C' & runValue(x)!='G']=0
	Rle(as.numeric(xres))
}

scoreG4hunt=function(y)
# y can be DNAString or a DNAStringSet or a simple char string.
{
	y2=Rle(strsplit(as.character(y),NULL)[[1]])
	y3=G4translate(y2)
	mean(y3)
}
