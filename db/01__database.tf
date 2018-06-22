provider "aws" {
  region = "us-east-1"
}

variable "db_password" {}

resource "aws_security_group" "database_access" {
  name_prefix = "database_access"
  ingress {
    from_port = 5432
    to_port = 5432
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  egress {
    from_port = 0
    to_port = 0
    protocol = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_db_instance" "progress_db" {
  identifier_prefix      = "progress-db"
  engine                 = "postgres"
  engine_version         = "10.1"
  instance_class         = "db.t2.micro"
  allocated_storage      = "10"
  storage_type           = "gp2"
  port                   = 5432
  publicly_accessible    = true
  username               = "auth"
  password               = "${var.db_password}"
  vpc_security_group_ids = ["${aws_security_group.database_access.id}"]
}

locals {
  db_uri = "postgres://auth:${var.db_password}@${aws_db_instance.progress_db.address}:5432/postgres"
}

output "DB_URI" {
  value = "${local.db_uri}"
}
