-module(try_wx).

-export([start/0]).

-include_lib("wx/include/wx.hrl").

start() ->
    Wx = wx:new(),
    F = wxFrame:new(Wx, -1, "Hello, Wallace"),
    wxFrame:show(F),

    statusbar(F),
    menubar(F),
    ok.

statusbar(F) ->
    wxFrame:createStatusBar(F),
    wxFrame:setStatusText(F, "no message yet."),

    S = wxFrame:getStatusBar(F),
    %sleep(2000),
    wxStatusBar:pushStatusText(S, "Hello, world"),
    %sleep(2000),
    wxStatusBar:popStatusText(S),
    ok.

menubar(F) ->
    MenuBar = wxMenuBar:new(),
    wxFrame:setMenuBar(F, MenuBar),
    M1 = wxMenu:new(),
    wxMenuBar:append(MenuBar, M1, "&File"),
    M1_1 = wxMenuItem:new([{id, 400}, {text, "&Quit"}]),
    wxMenu:append(M1, M1_1),
    E = wx:get_env(),
    spawn_link(fun () ->
                       wx:set_env(E),
                       listen_command(F)
               end),
    ok.

listen_command(F) ->
    wxFrame:connect(F, command_menu_selected),
    listen_loop(F).

listen_loop(F) ->
    receive
        #wx{id = 400} ->
            wxFrame:setStatusText(F, "trying to close window");
        #wx{id = Id} ->
            T = io_lib:format("unknown command (id: ~w)", [Id]),
            wxFrame:setStatusText(F, T)
    end,
    listen_loop(F).

sleep(Milliseconds) ->
    receive after Milliseconds -> ok end.
