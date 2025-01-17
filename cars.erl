% simulam o parcare 
% avem trei procese 
% Parcarea cu starile: open, full 
% Masina cu starile: parking, waiting 
% Poarta cu starile: open, closed 

% pentru fiecare proces am doua metode 
% - start_process: spawnez procesul si initializez starea 
% - process_loop: simulez automatul 

% comunicarea intre procese este realizata prin intermediul schimbului de mesaje 
% Pid ! message 
% Pid ! {request_entry, CarPid} 

% f(T1, T2,..., Tn) -> {f, T1, T2, ..., Tn}

% procesul pentru parcare 

% sintaxa: fun (arg) -> body end. 
start_parking(TotalSpaces) -> 
  spawn(fun () -> parking_loop(open, TotalSpaces, []) end).
  
parking_loop(State, SpacesLeft, Cars) ->
  io:format("Parcare: Stare ~p, locuri ramase: ~p, masini parcare: ~p~n", [State, SpacesLeft, Cars]),
  receive 
    {request_entry, CarPid} when State =:= open, SpacesLeft > 0 -> 
      io:format("Permit accesul masinii ~p~n", [CarPid]),
      CarPid ! {entry_granted, self()},
      parking_loop(State, SpacesLeft - 1, [CarPid | Cars]);
      
    {request_entry, CarPid} when State =:= open, SpacesLeft =:= 0 ->
      io:format("Parcarea este plina. Refuz accesul masinii ~p~n", [CarPid]),
      CarPid ! {entry_not_granted, self()},
      parking_loop(full, SpacesLeft, Cars);
    
    {car_exit, CarPid} ->
      io:format("Masina ~p a iesit din parcare~n", [CarPid]),
      NewCars = lists:delete(CarPid, Cars),
      NewSpaces = SpacesLeft + 1, 
      NewState = if NewSpaces > 0 -> open; true -> full end, 
      parking_loop(NewState, NewSpaces, NewCars);
      
    _ -> 
      io:format("Comanda necunoscuta pentru Parcare~n"),
      parking_loop(State, SpacesLeft, Cars)
  end.
  
% procesul pentru poarta 

start_gate() ->
  spawn(fun() -> gate_loop(closed) end).
  
gate_loop(State) -> 
  io:format("Poarta: stare ~p~n", [State]),
  receive 
    open_gate when State =:= closed -> 
      io:format("Deschidem poarta~n"),
      gate_loop(open);
      
    close_gate when State =:= open -> 
      io:format("Inchidem poarta~n"),
      gate_loop(closed);
      
    open_gate when State =:= open -> 
      io:format("Poarta este deja deschisa~n"),
      gate_loop(State);
      
    close_gate when State =:= closed -> 
      io:format("Poarta este deja inchisa~n"),
      gate_loop(State);
      
    _ -> 
      io:format("Comanda necunoscuta pentru Poarta~n"),
      gate_loop(State)
  end.

% procesul pentru masina 

start_car(ParkingPid, GatePid) ->
  spawn(fun () -> car_loop(waiting, ParkingPid, GatePid) end).
  
car_loop(State, ParkingPid, GatePid) ->
  io:format("Masina: stare ~p~n", [State]),
  case State of 
    waiting -> 
      ParkingPid ! {request_entry, self()},
      receive 
        {entry_granted, _ParkingPid} ->
          io:format("Acces permis. Cerem deschiderea portii~n"),
          GatePid ! open_gate, 
          io:format("Intrare in parcare~n"),
          GatePid ! close_gate, 
          car_loop(parking, ParkingPid, GatePid);
        
        _ -> 
          io:format("Acces refuzat. Asteptam...~n"),
          timer:sleep(1000),
          car_loop(waiting, ParkingPid, GatePid)
      end;
    
    parking -> 
      timer:sleep(3000), 
      io:format("Iesim din parcare~n"),
      ParkingPid ! {car_exit, self()},
      GatePid ! open_gate,
      io:format("Iesire din parcare pe poarta~n"),
      GatePid ! close_gate,
      car_loop(waiting, ParkingPid, GatePid)
    
  end. 
  
% main 

main(_) ->
  ParkingPid = start_parking(2),
  GatePid = start_gate(),
  
  Car1 = start_car(ParkingPid, GatePid),
  Car2 = start_car(ParkingPid, GatePid),
  Car3 = start_car(ParkingPid, GatePid),
  
  % Car1 ! {request_entry, ParkingPid},
  
  timer:sleep(3000).
