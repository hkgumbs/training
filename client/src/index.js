import 'bulma/css/bulma.css';
import '@fortawesome/fontawesome-free/js/all.js';
import auth0 from 'auth0-js';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const config = process.env.NODE_ENV === 'development' ? development() : production();
const node = document.getElementById('root');
const token = JSON.parse(localStorage.getItem('auth-token'));
const app = Main.embed(node, { dbapi: config.dbapi, token: token });

app.ports.outgoing.subscribe(function(msg) {
  switch (msg.tag) {
    case 'LOGIN':
      config.login();
      return;

    case 'LOGOUT':
      localStorage.removeItem('auth-token');
      config.logout();
      return;

    case 'SAVE-TOKEN':
      localStorage.setItem('auth-token', JSON.stringify(msg.data));
      return;
  }
});



// CONFIGURATION


function development() {
  const tokenRedirect = "/#id_token="
    + "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
    + ".eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI"
    + "6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDI"
    + "yLCJodHRwczovL3Bvc3RncmVzdC5jb20vcm9"
    + "sZSI6IndlYiJ9.6kByi7RXOhiJHDrITWZbwXe2YPEfh-x_nrkcPffHqjQ";
  return {
    dbapi: 'localhost:3001',
    login: () => window.location = tokenRedirect,
    logout: () => window.location.reload(),
  };
}

function production() {
  const initialUri = window.location.href.split('#')[0];
  const clientID = '2B20LQM-ze-J2iaVnvL6LsQ9zZ2I7oT7';
  const auth = new auth0.WebAuth({
    domain: 'progress-app.auth0.com',
    responseType: 'id_token',
    scope: 'openid',
    clientID: clientID,
    redirectUri: initialUri,
  });

  return {
    dbapi: 'https://progress-api.herokuapp.com/',
    login: () => auth.authorize(),
    logout: () => auth.logout({ clientID: clientID, returnTo: initialUri }),
  };
}

registerServiceWorker();
