data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)

convertBinToDec :: Integral a => a -> a
convertBinToDec 0=0
convertBinToDec x = 2 * convertBinToDec (div x 10) + (mod x 10)

replaceIthItem :: t -> [t] -> Int -> [t]
replaceIthItem _ [] _ = []
replaceIthItem  a (_:xs) 0 =  a : xs
replaceIthItem  a (x:xs) n = (x : replaceIthItem a xs (n-1))

splitEvery :: Int -> [a] -> [[a]]	 	
splitEvery _ [] = []
splitEvery n l = (take n l) : (splitEvery n (drop n l))

logBase2 :: Floating a => a -> a
logBase2 x= logBase 2 x
-- logBase2 1=0
-- logBase2 x= 1+ logBase2 (div x 2)

fillZeros :: [Char] -> Int -> [Char]
fillZeros x 0= x
fillZeros x n= "0" ++ fillZeros x (n-1)


getNumBits numOfSets  cacheType  cache = if cacheType == "setAssoc" then round(logBase2 numOfSets)
										 else if cacheType == "directMap" then round(logBase2( fromIntegral(length cache)))
										 else 0
												
										 								 	
search tag _ (-1)= NotPresent
search tag [] _= NotPresent
search tag ( It (T x) (D a) c f:xs) 0 = if tag==x && c==True then It (T x) (D a) True f
											else search tag xs (-1)
search tag ( It (T x) (D a) c f:xs) idx = 	search tag xs (idx-1)

search1 tag [] = NotPresent
search1 tag ( It (T x) (D a) c f:xs) = if tag==x && c==True then It (T x) (D a) True f
                                      else search1 tag xs
helper (It _ (D a) _  _ ) = a

				
convertAddress binAddress bitsNum cacheType     = if cacheType == "directMap" then (div (binAddress) (10^bitsNum),(mod binAddress (10^bitsNum)))
													else (div binAddress ( 10 ^(bitsNum)) ,mod binAddress (10^(bitsNum )))

	
getDataFromCache stringAddress cache cacheType bitsNum = if  cacheType == "directMap" then
																						if item == NotPresent
																						then NoOutput
																						else Out( helper item , 0)								
														 else 
																						  if  item1 == NotPresent
																						  then NoOutput
																						  else Out( helper item1 , hopsNum)
																															where {
																																(tag,idx) = convertAddress (read stringAddress :: Int) bitsNum "directMap" ;
																																 tagzeros  = (read (((fillZeros (show tag) (6-bitsNum-(length(show tag)))))) :: Int);
																																 item = search (tagzeros) (cache) (convertBinToDec idx); 
																																 (tag1,idx1) = convertAddress (read stringAddress :: Int) bitsNum "setAssoc" ;
																																 tagzeros1  = (read (((fillZeros (show tag1) (6-bitsNum-(length(show tag1)))))) :: Int);
																																 splittedcache = splitEvery (div (length cache) (2^bitsNum) ) cache;
																																 neededset = splittedcache!!idx1	;
																																 item1 = search1 (tagzeros1) (neededset);
																																 hopsNum = index (item1) (neededset) 0	}
older  (It _ _ False _)  _= True
older  (It _ _ True c) (It _ _ True f)= if c>=f
                                        then True
										else False
older  (It _ _ True _) (It _ _ False _)= False									

--oldestitem (It(a (b) False c):_) = It(a (b) False c)		
oldestitem [x] = x	
oldestitem (x:xs) = if (older (x) (oldestitem xs))  then x
                    else oldestitem xs

incrementclc []= []
incrementclc ((It a (b) bol c):xs) = if bol==True 
                                    then ((It a (b) bol (c+1)):(incrementclc xs))
								    else ((It a (b) bol c):(incrementclc xs))

index y (x:xs) acc= if y==x then acc
                    else index y xs (acc+1)																			

replaceInCache tag idx memory oldCache cacheType bitsNum = if cacheType == "directMap" then ((memory !! convertBinToDec (read (((fillZeros (show tag) (6-bitsNum-(length(show tag)))))++(fillZeros (show idx) (bitsNum-length(show idx)))) :: Int)) , replaceIthItem (It (T tag) (D (memory !! convertBinToDec (read (((fillZeros (show tag) (6-bitsNum-(length(show tag)))))++(fillZeros (show idx) (bitsNum-length(show idx)))) :: Int))) True 0) oldCache (convertBinToDec idx) )
                                                             else if cacheType=="fullyAssoc" then ( (memory !! convertBinToDec (read (((fillZeros (show tag) (6-bitsNum-(length(show tag)))))) :: Int))     ,   replaceIthItem (It (T tag) (D (memory !! convertBinToDec (read (((fillZeros (show tag) (6-bitsNum-(length(show tag)))))) :: Int))) True 0) (incrementclc oldCache) (index (oldestitem oldCache) oldCache 0 )) 
                                                             else  (itemdata , replaceIthItem (It (T tag) (D itemdata) True 0)  (concat(replaceIthItem (incrementclc neededset) splittedcache idx)) (index (oldestitem neededset) oldCache 0 )) 
																		where { itemdata     = (memory !! convertBinToDec (read (((fillZeros (show tag) (6-bitsNum-(length(show tag)))))++(fillZeros (show idx) (bitsNum-length(show idx)))) :: Int));
																				neededset     = (splitEvery (div (length oldCache) (2^bitsNum) ) oldCache)!!idx  ;
																				splittedcache = splitEvery (div (length oldCache) (2^bitsNum) ) oldCache }														
getData stringAddress cache memory cacheType bitsNum
													| x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
													| otherwise = (getX x, cache)
												where {
														x = getDataFromCache stringAddress cache cacheType bitsNum ;
														address = read stringAddress:: Int ;
														(tag, index) = convertAddress address bitsNum cacheType ;
														getX (Out (d, _)) = d }													
				
runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numOfSets =((d:prevData), finalCache)
														where {
																bitsNum = round(logBase2 numOfSets);
																(d, updatedCache) = getData addr cache memory cacheType bitsNum;
																(prevData, finalCache) = runProgram xs updatedCache memory cacheType numOfSets	}