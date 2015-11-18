 -module(diademo).  
   
 -compile(export_all). %% this is just for the demo, don't use this in production.  

add_one(X) -> 
   X+1.  

-spec(tuple_sum({number(),number(),number()})->number()).
tuple_sum({X,Y,Z}) ->  
   X+Y+Z.  


atom_add() ->  
   add_one(4).  

