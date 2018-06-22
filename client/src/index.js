import 'bulma/css/bulma.css';
import { Main } from './Main.elm';
import auth0 from 'auth0-js';
import registerServiceWorker from './registerServiceWorker';

const node = document.getElementById('root');
const token = JSON.parse(localStorage.getItem('auth-token'));
const dbapi = process.ENV === 'development'
  ? 'localhost:3001'
  : 'https://progress-api.herokuapp.com/';
const app = Main.embed(node, { dbapi, token });

const initialUri = window.location.href.split('#')[0];
const clientID = '2B20LQM-ze-J2iaVnvL6LsQ9zZ2I7oT7';
const auth = new auth0.WebAuth({
  domain: 'progress-app.auth0.com',
  responseType: 'id_token',
  scope: 'openid',
  clientID: clientID,
  redirectUri: initialUri,
});

app.ports.outgoing.subscribe(function(msg) {
  switch (msg.tag) {
    case 'LOGIN':
      auth.authorize();
      return;

    case 'LOGOUT':
      localStorage.removeItem('auth-token');
      auth.logout({ clientID: clientID, returnTo: initialUri });
      return;

    case 'SAVE-TOKEN':
      localStorage.setItem('auth-token', JSON.stringify(msg.data));
      return;
  }
});

registerServiceWorker();
