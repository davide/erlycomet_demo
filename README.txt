Intro
	
	This is a refactor for the erlycomet demos distributed with http://code.google.com/p/erlycomet/.
	I hacked away as I pleased so there are surely broken pieces lying around. :)

Dependencies:
	Put the beam files under the deps/<dependency>/ebin folder!
	* erlycomet -> http://github.com/davide/erlycomet/tree/master
	* Latest (@2009-01 - didn't check the version at the time and I usualy strip .svn folders :P)
		Mochiweb + patch (see details on erlycomet's REAME file dependencies)
	* Yaws 1.79 + patch (see details on erlycomet's REAME file dependencies)

Demos:

    * Simple echo
    * Server side counter
    * RPC (with custom serverside application)
    * Chat with one chatroom (/chat/demo), member tracking and private messages
    * Collaborative drawing (see below)


Collaborative drawing Example demo
==================================
1) install dojo, dojox and dijit (from SVN) into demo-docroot
   (because it won't work with dojo from AOL CDN)

2) apply this patch todojox.sketch.UndoStack.add:

+++ UndoStack.js
@@ -78,6 +78,9 @@
 				}
 			};
 			//console.log('dojox.sketch history add',state);
+			
+			dojox.cometd.publish("/chat/demo", {user: room._username, draw: state});
+			
 			this._steps.push(state);
 			this._undoedSteps = [];
 		},


