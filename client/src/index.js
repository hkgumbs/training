import 'bulma/css/bulma.css';
import '@fortawesome/fontawesome-free/js/all.js';
import '../vendor/google_api.js';
import auth0 from 'auth0-js';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const config = process.env.NODE_ENV === 'development' ? development() : production();
const node = document.getElementById('root');
const app = Main.embed(node);

gapi.load('client:auth2', () => {
  gapi.client.init({
    apiKey: "AIzaSyC2ejk1mVud3U0ncCqNjQ8okfWLUtpAjg0",
    clientId: "69197984052-n9dk5q6s609bfqd6cu5pns0u3qr81lii.apps.googleusercontent.com",
    discoveryDocs: ["https://sheets.googleapis.com/$discovery/rest?version=v4"],
    scope: "https://www.googleapis.com/auth/spreadsheets.readonly",
  }).then(() => {
  });
});

app.ports.outgoing.subscribe(function(msg) {
  switch (msg.tag) {
    case 'LOGIN':
      gapi.auth2.getAuthInstance().signIn();
      return;

    case 'LOGOUT':
      gapi.auth2.getAuthInstance().signOut();
      return;

    case 'GET-SHEET':
      gapi.client.sheets.spreadsheets.get({
        spreadsheetId: msg.data,
      }).then(response => {
        console.log("SUCCESS", response);
      }, response => {
        console.log("ERROR", response);
      });
      return;
  }
});



// CONFIGURATION


function development() {
  const tokenRedirect = "/#id_token="
    + "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
    + ".eyJyb2xlIjoid2ViIn0"
    + ".OV5FRcM6dZTHQR5oF0hcbmjBZdN2j-1QD-TSQ0ErD04";
  return {
    dbapi: 'http://localhost:3001',
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
    dbapi: 'https://progress-api.herokuapp.com',
    login: () => auth.authorize(),
    logout: () => auth.logout({ clientID: clientID, returnTo: initialUri }),
  };
}

registerServiceWorker();
