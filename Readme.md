# Workplace Bot to manage Active Directory groups

## Setup

To setup `greeting` and `getting started` messages, execute:

    curl -X POST -H "Content-Type: application/json" -d '{"get_started":{"payload":"/help"}}' "https://graph.facebook.com/v2.6/me/messenger_profile?access_token=${LDABOT_PAGE_TOKEN}"
    curl -X POST -H "Content-Type: application/json" -d '{"greeting": [{"locale":"default","text":"Hello! I will help you to manage your project groups."}]}' "https://graph.facebook.com/v2.6/me/messenger_profile?access_token=${LDABOT_PAGE_TOKEN}"

## Usage

Bot allow to:

* List members of any project group (inside `ProjectGroups` container).
* Add a new member into a project group. Only if requesting user is a manager (listed in `members` or `msExchCoManagedByLink` field).
* Remove existing member from a project group. Only if requesting user is a manager (listed in `members` or `msExchCoManagedByLink` field).
