data PTItem = Param Int | Text String

Text "wow"
Param 123

isText::PTItem -> Bool
isText (Text _) = True
isText (Param _) = False

isParam = not isText


