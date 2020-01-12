library(AER)
data( "Affairs", package = "AER" )
View(Affairs)
aff_tob <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating, data = Affairs)

summary(aff_tob)

