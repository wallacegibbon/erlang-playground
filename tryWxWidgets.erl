-module(tryWxWidgets).

-export([start/0]).

-include_lib("wx/include/wx.hrl").

start() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Hello, Wallace"),
    wxFrame:show(Frame),

    prvStatusBar(Frame),
    prvMenuBar(Frame),
    ok.

prvStatusBar(Frame) ->
    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame, "no message yet."),

    StatusBar = wxFrame:getStatusBar(Frame),
    %sleep(2000),
    wxStatusBar:pushStatusText(StatusBar, "Hello, world"),
    %sleep(2000),
    wxStatusBar:popStatusText(StatusBar),
    ok.

prvMenuBar(Frame) ->
    MenuBar = wxMenuBar:new(),
    wxFrame:setMenuBar(Frame, MenuBar),
    Menu = wxMenu:new(),
    wxMenuBar:append(MenuBar, Menu, "&File"),
    MenuItem = wxMenuItem:new([{id, 400}, {text, "&Quit"}]),
    wxMenu:append(Menu, MenuItem),
    Environment = wx:get_env(),
    spawn_link(fun () ->
                       wx:set_env(Environment),
                       prvListenCommand(Frame)
               end),
    ok.

prvListenCommand(F) ->
    wxFrame:connect(F, command_menu_selected),
    prvListenCommandLoop(F).

prvListenCommandLoop(F) ->
    receive
        #wx{id = 400} ->
            wxFrame:setStatusText(F, "trying to close window");
        #wx{id = Id} ->
            T = io_lib:format("unknown command (id: ~w)", [Id]),
            wxFrame:setStatusText(F, T)
    end,
    prvListenCommandLoop(F).

sleep(Milliseconds) -> receive after Milliseconds -> ok end.
