%% For more infomation about the wx module, you can see the demo by invoking `wx:demo()`
-module(try_wx_widgets).

-export([start/0]).

-include_lib("wx/include/wx.hrl").

start() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Hello, Wallace"),
    wxFrame:show(Frame),

    status_bar(Frame),
    menu_bar(Frame),
    ok.

status_bar(Frame) ->
    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame, "no message yet."),

    StatusBar = wxFrame:getStatusBar(Frame),
    %sleep(2000),
    wxStatusBar:pushStatusText(StatusBar, "Hello, world"),
    %sleep(2000),
    wxStatusBar:popStatusText(StatusBar),
    ok.

menu_bar(Frame) ->
    MenuBar = wxMenuBar:new(),
    wxFrame:setMenuBar(Frame, MenuBar),
    Menu = wxMenu:new(),
    wxMenuBar:append(MenuBar, Menu, "&File"),
    MenuItem = wxMenuItem:new([{id, 400}, {text, "&Quit"}]),
    wxMenu:append(Menu, MenuItem),
    Environment = wx:get_env(),
    spawn_link(fun() ->
                  wx:set_env(Environment),
                  listen_cmd(Frame)
               end),
    ok.

listen_cmd(F) ->
    wxFrame:connect(F, command_menu_selected),
    listen_cmd_loop(F).

listen_cmd_loop(F) ->
    receive
        #wx{id = 400} ->
            wxFrame:setStatusText(F, "trying to close window");
        #wx{id = Id} ->
            T = io_lib:format("unknown command (id: ~w)", [Id]),
            wxFrame:setStatusText(F, T)
    end,
    listen_cmd_loop(F).

sleep(Milliseconds) ->
    receive after Milliseconds ->
        ok
    end.
