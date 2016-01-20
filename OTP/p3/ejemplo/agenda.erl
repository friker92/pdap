-module(agenda).

-export([crear_bd/0, escribir_datos/0, consulta/1, fact/1]).

-include("agenda.hrl").
-include_lib("stdlib/include/qlc.hrl").


crear_bd() ->
        mnesia:create_schema([node()]),
        application:start(mnesia),
        mnesia:create_table(persona, [{attributes, record_info(fields, persona)}]),
        mnesia:create_table(telefono, [{attributes, record_info(fields, telefono)}, {type, bag}]),
        mnesia:create_table(dir_correo, [{attributes, record_info(fields, dir_correo)}, {type, bag}]),
        application:stop(mnesia).


transaccion_escribir_datos() ->
        mnesia:write(#persona{id = 1,
                               nombre = "Fernando Suárez", 
                               fecha_nacimiento = #fecha{dia = 1, mes = 12, anyo = 1976}}),
        mnesia:write(#persona{id = 2,
                               nombre = "Estela Ortega", 
                               fecha_nacimiento = #fecha{dia = 15, mes = 3, anyo = 1979}}),
        mnesia:write(#persona{id = 3,
                               nombre = "Jaime Barajas", 
                               fecha_nacimiento = #fecha{dia = 14, mes = 11, anyo = 1983}}),
        mnesia:write(#telefono{id_persona = 1, telefono = 918324112}),
        mnesia:write(#telefono{id_persona = 1, telefono = 629578221}),
        mnesia:write(#telefono{id_persona = 2, telefono = 912441245}),
        mnesia:write(#dir_correo{id_persona = 1, correo = "fernando.suarez@gmail.com"}),
        mnesia:write(#dir_correo{id_persona = 3, correo = "jaimebt@gmail.com"}),
        mnesia:write(#dir_correo{id_persona = 3, correo = "jaime.barajas@yahoo.es"}).
        
        
        
        
escribir_datos() ->
        mnesia:transaction(fun transaccion_escribir_datos/0).
                               
consulta(Q) ->
        {atomic, Result} = mnesia:transaction(fun() -> qlc:e(Q) end),
        Result.
        
fact(0) -> 1;
fact(N) when N >= 0 -> N * fact(N-1).                                            
