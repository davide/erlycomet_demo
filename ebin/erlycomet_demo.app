{application, erlycomet_demo,
  [
    {description, "Erlang Comet Server"},
    {vsn, "0.1"},
    {modules, [erlycomet_demo,
               erlycomet_demo_app,
			   erlycomet_demo_rpc,
			   erlycomet_demo_sup,
               erlycomet_demo_mochiweb_server,
               erlycomet_demo_tick_server,
               erlycomet_demo_yaws_server,
               erlycomet_demo_private_messenger,
               erlycomet_demo_inactive_connections_monitor,
			   erlycomet_demo_chatroom_user,
			   erlycomet_demo_user_monitor]},
    {registered, []},
    {applications, [kernel,
                    stdlib]},
    {included_applications, []},
    {env, []},
	{mod, {erlycomet_demo_app, [
		{ mochiweb_config, [
			{port, 3000},
			{docroot, "priv/mochiweb_docroot"}
			]},
		{ yaws_config, [
			  [ {port, 80},
				{servername, "localhost"},
				{listen, {0, 0, 0, 0}},
				{docroot, "priv/yaws_docroot"},
				{appmods, [{"/cometd", erlycomet_demo_yaws_server}]}]
			]}
		]}
	  }
]}.
    
     
