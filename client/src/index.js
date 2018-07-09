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

registerServiceWorker();
