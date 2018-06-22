provider "heroku" {}

variable "jwt_secret" {}

resource "heroku_app" "progress_db_api" {
  name   = "progress-db-api"
  region = "us"

  config_vars {
    POSTGREST_VER        = "0.5.0.0"
    PGRST_ROLE_CLAIM_KEY = ".\"https://postgrest.org/auth\""
    PGRST_JWT_SECRET     = "${var.jwt_secret}"
    DB_URI               = "${local.db_uri}"
    DB_SCHEMA            = "public"
    DB_ANON_ROLE         = "anon"
    DB_POOL              = "5"
  }

  buildpacks = [
    "https://github.com/hkgumbs/migrate-buildpack",
    "https://github.com/PostgREST/postgrest-heroku"
  ]
}
