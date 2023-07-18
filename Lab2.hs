-----------------------------------------------------------------------------------------

-- ASKHSH 1
sumInt::[Int]->Int
sumInt (s:t)=s + (sumInt t)
sumInt[] = 0


listMultiplication:: [Int]->[Int]->Int
listMultiplication a b = (sumInt a)*(sumInt b)


xsum'::[Int]->[Int]->Int
xsum' a (s:t) = (listMultiplication a (s:t)) + (xsum' ([s]++a) t)
xsum' a [] = 0


xsum :: [Int]->Int
xsum a = xsum' [] a





-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

    
trace':: (Int,Int)->(Int,Int)->[(Int,Int)]
trace' (ax,ay) (bx,by) = [(bx,by)]++y
    where
        ax'=
            if(bx<ax) then
                bx+1
            else
                if(bx>ax) then
                    bx-1
                else
                    bx
        ay'=
                if(by<ay) then
                    by+1
                else
                    if(by>ay) then
                        by-1
                    else
                        by
        y = 
            if((ax==bx&&ay==by))  then
                []
            else
                (trace'(ax,ay) (ax',ay'))


trace'' :: [(Int,Int)]->[(Int,Int)]
trace'' a = 
    if(length a>1)then
        (trace'' (drop 1 a))++trace' (a!!0) (a!!1)
    else
        []

trace :: [(Int,Int)]->[(Int,Int)]
trace matrix = ((trace'' a))
        where
            a=[(0,0)]++matrix++[(0,0)]



-----------------------------------------------------------------------------------------
     
-- ASKHSH 3

onHead :: (a -> a) -> [a] -> [a]
onHead _ [] = []
onHead f (x:xs) = f x:xs

combos :: [a] -> [[[a]]]
combos [] = [[]]
combos [x] = [[[x]]]
combos (x:xs) = [([x]:), onHead (x:)] <*> combos xs

partition :: String->[[String]]
partition x = (combos x)





-----------------------------------------------------------------------------------------
     
-- ASKHSH 4


help::Integer->Integer->[Integer->Integer]->Integer
help i n (head:tail)= ((head (n-i+1))`div`second+(help (i+1) n tail))
            where
                second=(2^(i-1))

help _ _ []=(0)


hof :: [Integer->Integer] ->(Integer->Integer)
hof s = \n -> help 1 n s

-----------------------------------------------------------------------------------------
     
-- ASKHSH 5                                   

combine::[u]->[v]->(u->v->w)->(u->v->w)->(Int->Bool)->[w]
combine a b f g h = combine' (length a) a b f g h

combine' :: Int->[u]->[v]->(u->v->w)->(u->v->w)->(Int->Bool)->[w]

combine' originalSize (s:t) (a:b) f g h = [c1]++(combine' originalSize t b f g h)
            where
                c1= 
                    if(h (originalSize- (length t)))then
                        (f s a)
                    else
                        (g s a)

combine' originalSize [] (a:b) f g h=[]

combine' originalSize (s:t) [] f g h=[]

combine' originalSize [] [] f g h=[]
        
        --[g s a]++(combine' originalSize t b f g h)
--combine' originalSize [] [] f g h=[]

charToString :: Char -> String
charToString = (:[])

main = do
    putStrLn("---------Excersise 1------------")
    print(xsum [4,5,8])
    print( xsum [10,3,12,7])
    print(xsum [10,7,4,8,12])
    print(xsum [1..10])
    print(xsum [1..100])
    putStrLn("---------Excersise 2------------")
    print(trace [(2 , 2), (3 , 5)])
    putStrLn("---------Excersise 3------------")
    --print( partition "kwlofardoskwlofardos")
    putStrLn("---------Excersise 4------------")
    print(map (hof [(+1)]) [1..10])
    print(map (hof [(+1),(+2)]) [1..10])
    --print( map (hof [(2^),(2^),(2^),(2^),(2^)]) [5..12])
<<<<<<< refs/remotes/origin/excersise2

    putStrLn("---------Excersise 5------------")
    print(combine [5,4,3,2] [7,8,9,10] (*) (^) odd)
    print(combine ["summer","drops","black","white"] ["time","rain","board","snow"] (++) (\x y -> y++x) odd)
    print(combine ["summer","drops","black","white","time","rain","board","snow"] [1..] (\x y -> (x,0)) (\x y -> ("",y)) even)
=======
    print (combine (length [5,4,3,2]) [5,4,3,2] [7,8,9,10] (*) (^) odd)
    print(['s','u','n'])
>>>>>>> updated
