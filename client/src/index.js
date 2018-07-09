import 'bulma/css/bulma.css';
import '@fortawesome/fontawesome-free/js/all.js';
import '../vendor/google_api.js';
import auth0 from 'auth0-js';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const googleApiKey = "AIzaSyC2ejk1mVud3U0ncCqNjQ8okfWLUtpAjg0";
const node = document.getElementById('root');
const app = Main.embed(node);

gapi.load('client:auth2', () => {
  gapi.client.init({
    apiKey: googleApiKey,
    clientId: "69197984052-n9dk5q6s609bfqd6cu5pns0u3qr81lii.apps.googleusercontent.com",
    discoveryDocs: ["https://sheets.googleapis.com/$discovery/rest?version=v4"],
    scope: "https://www.googleapis.com/auth/spreadsheets.readonly",
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

    case 'PICK-SHEET':
      gapi.load('picker', () => {
        const token = gapi.auth2.getAuthInstance().currentUser.get()
          .getAuthResponse(true).access_token;
        new google.picker.PickerBuilder()
          .addView(google.picker.ViewId.SPREADSHEETS)
          .setOAuthToken(token)
          .setDeveloperKey(googleApiKey)
          .setCallback(data => {
            if (data.action === "picked") {
              gapi.client.sheets.spreadsheets.get({
                spreadsheetId: data.docs[0].id,
              }).then(
                response => console.log("SUCCESS", response),
                response => console.log("ERROR", response));
            }
          })
          .build()
          .setVisible(true);
      });
      return;
  }
});

registerServiceWorker();
