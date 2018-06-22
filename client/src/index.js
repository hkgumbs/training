import 'bulma/css/bulma.css';
import { Main } from './Main.elm';
import auth0 from 'auth0-js';
import registerServiceWorker from './registerServiceWorker';

var node = document.getElementById('root');
var token = JSON.parse(localStorage.getItem('auth-token'));
var app = Main.embed(node, token);

var initialUri = window.location.href.split("#")[0];
var clientID = '2B20LQM-ze-J2iaVnvL6LsQ9zZ2I7oT7';
var auth = new auth0.WebAuth({
  domain: 'progress-app.auth0.com',
  responseType: 'id_token',
  scope: 'openid',
  clientID: clientID,
  redirectUri: initialUri,
});

app.ports.outgoing.subscribe(function(msg) {
  switch (msg.tag) {
    case "LOGIN":
      auth.authorize();
      return;

    case "LOGOUT":
      localStorage.removeItem('auth-token');
      auth.logout({ clientID: clientID, returnTo: initialUri });
      return;

    case "SAVE-TOKEN":
      localStorage.setItem('auth-token', JSON.stringify(msg.data));
      return;
  }
});

registerServiceWorker();
