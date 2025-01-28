-module(main).
-export([main/1]).

start_cat(Feeder, CatID) ->
    spawn(fun() -> cat_waiting(Feeder, CatID) end).

cat_waiting(Feeder, CatID) ->
    % ask for food
    io:format("Cat ~p is requesting food.~n", [CatID]),
    Feeder ! {request_food, CatID, self()},

    % wait for feeder to send food
    io:format("Cat ~p is waiting for response to request.~n", [CatID]),
    receive
        food ->
            cat_feeding(Feeder, CatID);
        _ ->
            io:format("Error at cat ~p: this is not food!~n", [CatID]),
            cat_waiting(Feeder, CatID)
    end.

cat_feeding(Feeder, CatID) ->
    io:format("Cat ~p is eating.~n", [CatID]),

    % It takes at most 3 seconds for a cat to finish eating.
    % Simulate eating by waiting a random amount of time between 0.5 and 3 seconds.
    timer:sleep(round(rand:uniform() * 2500) + 500),

    io:format("Cat ~p is done eating.~n", [CatID]),
    Feeder ! done_eating,

    timer:sleep(500),
    cat_waiting(Feeder, CatID).

feeder_idle() ->
    io:format("Feeder is idle. Waiting for requests...~n"),
    receive
        {request_food, CatID, From} ->
            io:format("Feeder now feeding cat ~p.~n", [CatID]),
            From ! food,
            feeder_feeding([]);
        _ ->
            io:format("Invalid request received while idle!~n")
    end.

feeder_feeding(PendingRequests) ->
    io:format("Current queue: ~w~n", [PendingRequests]),
    receive
        {request_food, CatID, From} ->
            io:format("Feeder received request from cat ~p while already feeding. Added to queue.~n", [CatID]),
            feeder_feeding([From | PendingRequests]);
        done_eating when PendingRequests =:= [] ->
            io:format("Cat is done eating. No pending requests.~n"),
            feeder_idle();
        done_eating -> 
            io:format("Cat is done eating. Feeding next cat.~n"),
            NextCat = lists:last(PendingRequests),
            NextList = lists:delete(NextCat, PendingRequests),
            NextCat ! food,
            feeder_feeding(NextList);
        _ ->
            io:format("Invalid request received while feeding!~n")
    end.

main(_) ->
    Feeder = spawn(fun() -> feeder_idle() end),
    start_cat(Feeder, 1),
    start_cat(Feeder, 2),
    start_cat(Feeder, 3),
    start_cat(Feeder, 4),
    start_cat(Feeder, 5),
    timer:sleep(20000).
