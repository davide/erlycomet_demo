var room = {
	_last: "",
	_username: null,

	join: function(name){
		if(name == null || name.length==0 ){
			alert('Please enter a username!');
		}else{

			this._username=name;
			dojo.byId('join').className='hidden';
			dojo.byId('joined').className='';
			dojo.byId('phrase').focus();

			// subscribe and join
			dojox.cometd.startBatch();
			dojox.cometd.subscribe("/chat/demo", room, "_chat");
			dojox.cometd.publish("/chat/demo", { user: room._username, join: true, chat : room._username+" has joined"});
			dojox.cometd.endBatch();

			// handle cometd failures while in the room
			room._meta=dojo.subscribe("/cometd/meta",function(event){
				console.debug(event);   
				if (event.action=="handshake") {
					room._chat({data:{join:true,user:"SERVER",chat:"reconnected"}});
					dojox.cometd.subscribe("/chat/demo", room, "_chat");
				} else if (event.action=="connect" && !event.successful) {
					room._chat({data:{leave:true,user:"SERVER",chat:"disconnected!"}});
			}
			});
		}
	},

	leave: function(){
		if (room._username==null) {
			return;
		}

		if (room._meta) {
			dojo.unsubscribe(room._meta);
		}
		room._meta=null;

		dojox.cometd.startBatch();
		dojox.cometd.unsubscribe("/chat/demo", room, "_chat");
		dojox.cometd.publish("/chat/demo", { user: room._username, leave: true, chat : room._username+" has left"});
		dojox.cometd.endBatch();

		// switch the input form
		dojo.byId('join').className='';
		dojo.byId('joined').className='hidden';
		dojo.byId('username').focus();
		room._username=null;
		dojo.byId('members').innerHTML="";
	},

	chat: function(text){
		if (!text || !text.length) {
			return false;
		}
		var colons = text.indexOf("::");
		if (colons > 0) {
			var peer = text.substring(0, colons);
			text = text.substring(colons + 2);
			if (!text.length) {
				return false;
			}
			dojox.cometd.publish("/service/privatechat", {
				room: "/chat/demo", // This should be replaced by the room name
				chat: text,
				peer: peer
			});
		} else {
			dojox.cometd.publish("/chat/demo", {
				user: room._username,
				chat: text
			});
		}
	},

	_chat: function(message){
		var chat=dojo.byId('chat');
		if(!message.data){
			alert("bad message format "+message);
			return;
		}
		
		if (message.data.members) {
			var members = dojo.byId('members');
			var list="";
			for (var i=0; i < message.data.members.length; i++)
				list+=message.data.members[i]+"<br/>";
			members.innerHTML=list;
		} else {
			var chat = dojo.byId('chat');
			var from = message.data.user;
			var membership = message.data.join || message.data.leave;
			var text = message.data.chat;
			if (!text) return;

			if (!membership && from == room._last) {
				from = "...";
			} else {
				room._last = from;
				from += ":";
			}

			if (membership) {
				chat.innerHTML += "<span class=\"membership\"><span class=\"from\">" + from + "&nbsp;</span><span class=\"text\">" + text + "</span></span><br/>";
				room._last = "";
			} else if (message.data.scope == "private") {
				chat.innerHTML += "<span class=\"private\"><span class=\"from\">" + from + "&nbsp;</span><span class=\"text\">[private]&nbsp;" + text + "</span></span><br/>";
			} else {
				chat.innerHTML += "<span class=\"from\">" + from + "&nbsp;</span><span class=\"text\">" + text + "</span><br/>";
			}
			chat.scrollTop = chat.scrollHeight - chat.clientHeight;
		}
	},

	_init: function(){
		dojo.byId('join').className='';
		dojo.byId('joined').className='hidden';
		dojo.byId('username').focus();

		var element=dojo.byId('username');
		element.setAttribute("autocomplete","OFF"); 
		dojo.connect(element, "onkeyup", function(e){   
			if(e.keyCode == dojo.keys.ENTER){
				room.join(dojo.byId('username').value);
				return false;
			}
			return true;
		});

		element=dojo.byId('joinB');
		element.onclick = function(){
			room.join(dojo.byId('username').value);
			return false;
		}

		element=dojo.byId('phrase');
		element.setAttribute("autocomplete","OFF");
		dojo.connect(element, "onkeyup", function(e){   
			if(e.keyCode == dojo.keys.ENTER){
				room.chat(dojo.byId('phrase').value);
				dojo.byId('phrase').value='';
				return false;
			}
			return true;
		});

		element=dojo.byId('sendB');
		element.onclick = function(){
			room.chat(dojo.byId('phrase').value);
			dojo.byId('phrase').value='';
		}

		element=dojo.byId('leaveB');
		element.onclick = function(){
			room.leave();
		}
	} 
};