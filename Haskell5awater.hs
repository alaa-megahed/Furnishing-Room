import FurnitureResources
import Data.List
import Data.Maybe


statsList = sorting (statsListHelper training [])

statsListHelper [] furnStats = furnStats
statsListHelper (room:rooms) furnStats = statsListHelper rooms (generate room furnStats) 

insert2 (f, pos, freq) [] = [(f, pos, freq)]
insert2 (f, pos, freq) ((f2, pos2, freq2):fs) = if(freq2 < freq) then (f, pos, freq):(f2, pos2, freq2):fs
												else (f2, pos2, freq2):(insert2 (f, pos, freq) fs)
-- sort the list of "right" or "below" for every furniture element
insertionSort [] = [] 
insertionSort (x:xs) = insert2 x (insertionSort xs)

sorting [] = []
sorting ((a, [right, below]):fs) = ((a, [insertionSort right, insertionSort below]):(sorting fs))


-- f => furniture piece whose statistics we're searching 
-- b => the other furniture piece 
findFurnitureUpdate :: [Char] -> [Char] -> [Char] -> [([Char],[[([Char],[Char],Int)]])]-> [([Char],[[([Char],[Char],Int)]])]
findFurnitureUpdate a b "right" [] = [(a, [[(b, "right", 1)], []])]
findFurnitureUpdate a b "below" [] = [(a, [[], [(b, "below", 1)]])]
findFurnitureUpdate a b c ((f, [right, below]):xs) | (a == f && c == "right") = ((a,[(findFurnitureUpdateHelper b c right), below]):xs) 
								           
                                                   | (a==f && c == "below") = ((a, [right, (findFurnitureUpdateHelper b c below)]) :xs)
								                   | otherwise = [(f, [right, below])] ++ (findFurnitureUpdate a b c xs)

findFurnitureUpdateHelper b "right" [] = [(b, "right", 1)]
findFurnitureUpdateHelper b "below" [] = [(b, "below", 1)]

findFurnitureUpdateHelper b "right" ((otherf, "right", freq):right) = if(b == otherf) then ((b, "right", freq +1):right)
										else 
											(otherf, "right", freq):(findFurnitureUpdateHelper b "right" right)

findFurnitureUpdateHelper b "below" ((otherf, "below", freq):below) = if(b == otherf) then ((otherf, "below", freq+1):below) 
										else 
											(otherf, "below", freq):(findFurnitureUpdateHelper b "below" below) 
generate [[f]] furnStats = furnStats
generate ([f]:r2:rs) furnStats = generate (r2:rs) (findFurnitureUpdate f (last r2) "below" furnStats)
generate [(f1:f2:fs)] furnStats = generate [(f2:fs)] (findFurnitureUpdate f1 f2 "right" furnStats) 
generate ((f1:f2:fs):r2:rs) furnStats  = generate ((f2:fs):r2:rs) (findFurnitureUpdate f1 f2 "right" updated_furnStats) 
 							      where updated_furnStats = (findFurnitureUpdate f1 (getElement ((length r2)-(length (f1:f2:fs))) r2 ) "below" furnStats )


getElement index l =  getElementHelper 0 index l 
getElementHelper counter index (x:xs) | (counter == index) = x | otherwise = getElementHelper (counter+1) (index) (xs)




getFurnStat f = (getFurnStatHelper f statsList) 

getRight f = head (getFurnStat f)
getBelow f = last (getFurnStat f)

getFurnStatHelper f [] = [] 
getFurnStatHelper f ((x, stats):xs) = if (f==x) then stats else getFurnStatHelper f xs 


getPossibleNeighbour left top =  getElement ( randomZeroToX ( (length x)-1 ) ) x where x = freqHelp (left ++ top)



freqHelp []=[]
freqHelp ((f, pos, freq):xs) = replicate freq f ++ (freqHelp xs)

furnishRoom n f = helpFurnish n firstRow [] [firstRow] where firstRow = buildFirstRow n [f]

buildFirstRow n firstRow = if (length firstRow < n) then buildFirstRow n (firstRow ++ [(getPossibleNeighbour (getRight (last firstRow)) []) ]) 
							else firstRow 

helpFurnish n prev row room | ((length room) == n) = room
						 |  ((length row) == 0) = (helpFurnish n prev (row ++[getPossibleNeighbour []  (getBelow(head prev))]) room) 
											
						 |  ( ((length row) /= 0) && (length row < n) )  = (helpFurnish n prev (row ++ [getPossibleNeighbour (getRight (last row)) (getBelow (getElement (n - (length row) ) prev) ) ]) room) 
					
						 | otherwise = helpFurnish n row [] (room++[row]) 
											


