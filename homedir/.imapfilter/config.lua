#!/usr/bin/env lua

-- inspect = require "inspect"

function decrypt_password(account)
  local cmd = string.format("gpg2 --for-your-eyes-only --quiet --batch -d ~/.passwd/%s.gpg",
                            account);

  local handle = io.popen(cmd)
  local result = handle:read("*a")
  handle:close()
  return result
end

local zimpler_account = IMAP {
  server = 'imap.gmail.com',
  username = 'jonas.collberg@zimpler.com',
  password = decrypt_password("zimpler"),
  ssl = 'tls'
}

daily_reports = zimpler_account.INBOX:contain_from("notifier@zimpler.com")
daily_reports:delete_messages()

zimpler_sandbox = zimpler_account.INBOX:contain_from("sandbox+noreply@zimpler.com")
zimpler_sandbox:delete_messages()

honeybadger_notifications = zimpler_account.INBOX:contain_from("Honeybadger Notifications")
honeybadger_notifications:delete_messages()

honeybadger_invoices = zimpler_account.INBOX:contain_from("support@honeybadger.io") *
                       zimpler_account.INBOX:contain_subject("Your Honeybadger invoice")

honeybadger_invoices:delete_messages()

stackdriver_alerts = zimpler_account.INBOX:contain_from("Stackdriver Alerts")
stackdriver_alerts:delete_messages()

monit_alert = zimpler_account.INBOX:contain_subject("monit alert -- ")
monit_alert:delete_messages()

gandi = zimpler_account.INBOX:contain_from("gandi.net")
gandi:delete_messages()

viper_prod = zimpler_account.INBOX:contain_from("viper-production@zimpler.com")
viper_prod:delete_messages()

pingdom = zimpler_account.INBOX:contain_from("support@pingdom.com")
pingdom:delete_messages()

skylight_trends = zimpler_account.INBOX:contain_from("trends@skylight.io")
skylight_trends:delete_messages()

customer_io = zimpler_account.INBOX:contain_from("win@customer.io")
customer_io:delete_messages()

trello_receipts = zimpler_account.INBOX:contain_from("support@trello.com") *
                  zimpler_account.INBOX:contain_subject("Your Trello Account")
trello_receipts:delete_messages()

bearcom_helpdesk = zimpler_account.INBOX:contain_from("helpdesk@bearcom.se")
bearcom_helpdesk:mark_seen()

accounts_list = zimpler_account.INBOX:contain_to("accounts@zimpler.com") +
                zimpler_account.INBOX:contain_to("accounts@pugglepay.com") +
                zimpler_account.INBOX:contain_to("berlin@zimpler.com")
accounts_list:mark_seen()

dev_list = zimpler_account.INBOX:contain_to("dev@zimpler.net") +
           zimpler_account.INBOX:contain_to("dev@zimpler.com") +
           zimpler_account.INBOX:contain_to("dev@pugglepay.com")
dev_list:mark_seen()
