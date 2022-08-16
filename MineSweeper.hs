type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving( Show ,Eq)

up:: MyState -> MyState
up (S (x,y) s s1 s2 ) = if x>0 then (S (x-1,y) s "up" (S (x,y) s s1 s2 ) )
						else 	Null
down:: MyState -> MyState
down (S (x,y) s s1 s2 ) = if x<7 then (S (x+1,y) s "down" (S (x,y) s s1 s2 ) )
						else 	Null					
left:: MyState -> MyState
left (S (x,y) s s1 s2 ) = if y>0 then (S (x,y-1) s "left" (S (x,y) s s1 s2 ) )
						else 	Null
right:: MyState -> MyState
right (S (x,y) s s1 s2 ) = if y<7 then (S (x,y+1) s "right" (S (x,y) s s1 s2 ) )
						else 	Null
collect:: MyState -> MyState
collect (S (x,y) s s1 s2) = if((filter (/=(x,y)) s)==s) then Null
							else (S (x,y) (filter (/=(x,y)) s) "collect" (S (x,y) s s1 s2))

isGoal::MyState->Bool
isGoal (S (x,y) s s1 s2 ) = if(s==[]) then True else False

isCollect::MyState->Bool
isCollect (S (x,y) s s1 s2 ) = if(s1=="collect") then True else False

constructSolution:: MyState ->[String]
constructSolution s = constructHelper s []

constructHelper :: MyState -> [String] -> [String]
constructHelper Null list = filter (/="") list
constructHelper (S (x,y) s s1 s2) list =  constructHelper s2 (s1:list)
			  
minPoint _ [] _ p= p
minPoint (a,b) ((x,y):t) c p |(abs (a-x)+ abs(b-y)) < c = minPoint (a,b) t (abs (a-x)+ abs(b-y)) (x,y)
					         |otherwise = minPoint (a,b) t c p

sortList p list = sortHelper p list []					 
				 
sortHelper _ [] list = list
sortHelper (a,b) ((x,y):t) list = sortHelper (a,b) (filter (/=(minPoint (a,b) ((x,y):t) (abs (a-x)+ abs(b-y)) (x,y))) ((x,y):t)) (list++[(minPoint (a,b) ((x,y):t) (abs (a-x)+ abs(b-y)) (x,y))])
							
nextMyStates::MyState->MyState
nextMyStates (S (a,b) [] s1 s2) = (S (a,b) [] s1 s2)
nextMyStates (S (a,b) ((x,y):t) s1 s2) | x>a && ((down (S (a,b) ((x,y):t) s1 s2))/=Null) = down (S (a,b) ((x,y):t) s1 s2)
									   | x<a && ((up (S (a,b) ((x,y):t) s1 s2))/=Null) = up (S (a,b) ((x,y):t) s1 s2)
									   | a==x && y>b && ((right (S (a,b) ((x,y):t) s1 s2))/=Null) = right (S (a,b) ((x,y):t) s1 s2)
									   | a==x &&  y<b && ((left (S (a,b) ((x,y):t) s1 s2))/=Null) = left (S (a,b) ((x,y):t) s1 s2)
									   |otherwise = collect (S (a,b) ((x,y):t) s1 s2)

solve :: Cell->[Cell]->[String]
solve p s = constructSolution (solveHelper(S p (sortList p s) "" Null))

solveHelper:: MyState->MyState
solveHelper s |isGoal s = s
			  |otherwise = solveHelper (nextMyStates s)						 


     

