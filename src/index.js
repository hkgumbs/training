import "bulma/css/bulma.css";
import "@fortawesome/fontawesome-free/js/all.js";
import "../vendor/google_api.js";
import auth0 from "auth0-js";
import { Main } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const node = document.getElementById("root");
const googleApiKey = "AIzaSyC2ejk1mVud3U0ncCqNjQ8okfWLUtpAjg0";

const showError = () => {
  node.innerHTML = `
    <div class="container">
      <div class="section">
        <div class="notification is-danger">
          We weren't able to connect to your exercises.
          <strong>Refresh the page to try again.</strong>
        </div>
      </div>
    </div>
  `;
};

const getExercises = spreadsheet => {
  const sheets = spreadsheet.result.sheets
      .filter(sheet => !sheet.properties.title.startsWith("_"));
  return gapi.client.sheets.spreadsheets.values.batchGet({
    spreadsheetId: spreadsheet.result.spreadsheetId,
    ranges: sheets.map(sheet => `${sheet.properties.title}!A1:Z100`),
  }).then(grids => ({
    document: spreadsheet.result.properties.title,
    names: sheets.map(sheet => sheet.properties.title),
    exercises: grids.result.valueRanges.map(range => range.values),
  }));
};

const launchPicker = auth => {
  gapi.load("picker", () => {
    new google.picker.PickerBuilder()
      .addView(google.picker.ViewId.SPREADSHEETS)
      .setOAuthToken(auth.currentUser.get().getAuthResponse(true).access_token)
      .setDeveloperKey(googleApiKey)
      .setCallback(data => {
        if (data.action === "loaded") return;
        if (data.action === "cancel") return showError();
        gapi.client.sheets.spreadsheets
          .get({ spreadsheetId: data.docs[0].id })
          .then(getExercises)
          .then(exercises => { node.innerHTML = ""; Main.embed(node, exercises) })
          .catch(showError);
      })
      .build()
      .setVisible(true);
  });
};

gapi.load("client:auth2", () => {
  gapi.client.init({
    apiKey: googleApiKey,
    clientId: "69197984052-n9dk5q6s609bfqd6cu5pns0u3qr81lii.apps.googleusercontent.com",
    discoveryDocs: ["https://sheets.googleapis.com/$discovery/rest?version=v4"],
    scope: "https://www.googleapis.com/auth/spreadsheets.readonly",
  }).then(() => {
    const auth = gapi.auth2.getAuthInstance();
    const config = { ux_mode: "redirect" };
    auth.isSignedIn.get()
      ? launchPicker(auth)
      : auth.signIn(config).then(() => launchPicker(auth), showError);
  }).catch(showError);
});

registerServiceWorker();
