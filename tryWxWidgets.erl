-module(tryWxWidgets).

%% For more infomation about the wx module, you can see the demo by invoking `wx:demo()`

-export([start/0]).

-include_lib("wx/include/wx.hrl").

start() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Hello, Wallace"),
    wxFrame:show(Frame),

    statusBar(Frame),
    menuBar(Frame),
    ok.

statusBar(Frame) ->
    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame, "no message yet."),

    StatusBar = wxFrame:getStatusBar(Frame),
    %sleep(2000),
    wxStatusBar:pushStatusText(StatusBar, "Hello, world"),
    %sleep(2000),
    wxStatusBar:popStatusText(StatusBar),
    ok.

menuBar(Frame) ->
    MenuBar = wxMenuBar:new(),
    wxFrame:setMenuBar(Frame, MenuBar),
    Menu = wxMenu:new(),
    wxMenuBar:append(MenuBar, Menu, "&File"),
    MenuItem = wxMenuItem:new([{id, 400}, {text, "&Quit"}]),
    wxMenu:append(Menu, MenuItem),
    Environment = wx:get_env(),
    spawn_link(fun () ->
                       wx:set_env(Environment),
                       listenCommand(Frame)
               end),
    ok.

listenCommand(F) ->
    wxFrame:connect(F, command_menu_selected),
    listenCommandLoop(F).

listenCommandLoop(F) ->
    receive
        #wx{id = 400} ->
            wxFrame:setStatusText(F, "trying to close window");
        #wx{id = Id} ->
            T = io_lib:format("unknown command (id: ~w)", [Id]),
            wxFrame:setStatusText(F, T)
    end,
    listenCommandLoop(F).

sleep(Milliseconds) ->
    receive after Milliseconds -> ok end.
