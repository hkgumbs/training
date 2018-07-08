require "json"
require "sinatra"
require "google_drive"

# CONFIGURATION

CONFIG = JSON.parse File.read("config.json")
AUTH = Google::Auth::UserRefreshCredentials.new(
  client_id: CONFIG.fetch("client_id"),
  client_secret: CONFIG.fetch("client_secret"),
  scope: [
    "https://www.googleapis.com/auth/drive",
    "https://spreadsheets.google.com/feeds/",
  ],
  redirect_uri: "http://localhost:4567/oauth")


# API

def exercise(worksheet)
  return nil if worksheet.title =~ /^_/
  {
    name: worksheet.title,
    features: [], # TODO
    movements: worksheet.rows(1).map do |row|
      {
        name: row[0],
        sets: row[1].to_i,
        reps: row[2].to_i,
        load: row[3],
        rest: row[4].to_i,
        progression_rate: row[5].to_i,
      }
    end,
  }
end


# WEB

$session = nil

get "/" do
  $session ? "OK" : redirect(AUTH.authorization_uri)
end

get "/oauth" do
  AUTH.code = request["code"]
  AUTH.fetch_access_token!
  $session = GoogleDrive::Session.from_credentials(AUTH)
  redirect to("/")
end

get "/api/exercises", provides: "json" do
  $session.
    file_by_title("Exercises").
    worksheets.
    map { |worksheet| exercise worksheet }.compact.to_json
end
