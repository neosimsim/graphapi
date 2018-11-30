package json

import (
	"strings"
	"testing"
)

func TestReadPropertyInfo(t *testing.T) {
	if info, err := ReadPropertyInfo(strings.NewReader(order)); err != nil {
		t.Fatal(err)
	} else {
		if info["id"].Type != "string" {
			t.Errorf("expected 'id' to be a string: %s", info["id"].Type)
		}
		if info["id"].Editable {
			t.Error("expected 'id' not to be editable")
		}
		if info["id"].Visible {
			t.Error("expected 'id' not to be visible")
		}
		if info["type"].Type != "predefined" {
			t.Errorf("expected 'type' to be apredefined: %s", info["type"].Type)
		}
		if info["type"].Editable {
			t.Error("expected 'type' not to be editable")
		}
		if info["type"].Visible {
			t.Error("expected 'type' not to be visible")
		}
		if info["title"].Type != "string" {
			t.Errorf("expected 'title' to be apredefined: %s", info["title"].Type)
		}
		if info["title"].Editable {
			t.Error("expected 'title' not to be editable")
		}
		if info["title"].Visible {
			t.Error("expected 'title' not to be visible")
		}
		if info["itemName"].Type != "string" {
			t.Errorf("expected 'itemName' to be a string: %s", info["itemName"].Type)
		}
		if info["orderTime"].Type != "dateTime" {
			t.Errorf("expected 'orderTime' to be a date: %s", info["orderTime"].Type)
		}
		if info["isSend"].Type != "boolean" {
			t.Errorf("expected 'isSend' to be a boolean: %s", info["isSend"].Type)
		}
		if info["amount"].Type != "integer" {
			t.Errorf("expected 'amount' to be a integer: %s", info["amount"].Type)
		}
		if info["transportation"].Type != "predefined" {
			t.Errorf("expected 'transportation' to be a predefined: %s", info["transportation"].Type)
		}
		if info["ingredients"].Type != "subset" {
			t.Errorf("expected 'ingredients' to be a subset: %s", info["ingredients"].Type)
		}
		if info["notificationMails"].Type != "set" {
			t.Errorf("expected 'notificationMails' to be a set: %s", info["notificationMails"].Type)
		}
	}
}

const order = `
{
  "definitions": {},
  "$schema": "http://verinice.com/veo/draft-01/schema#",
  "type": "object",
  "title": "Order-Schema",
  "description": "A sample schema to display all possibilities of the meta schema.",
  "properties": {
    "id": {
      "type": "string",
      "title": "The UUID to identify the element"
    },
    "type": {
      "type": "string",
      "title": "Type name",
      "description": "The name of the type described by this schema",
      "enum": [
        "order"
      ]
    },
    "title": {
      "type": "string",
      "title": "An Order"
    },
    "itemName": {
      "type": "string",
      "title": "The name of the ordered item",
      "description": "An example property of type string"
    },
    "orderTime": {
      "type": "string",
      "title": "The time the order has been made",
      "format": "date-time",
      "description": "An example property of type dateTime"
    },
    "isSend": {
      "type": "boolean",
      "title": "Whether the item has been send",
      "description": "An example property of type boolean"
    },
    "price": {
      "type": "number",
      "title": "The total price of the order",
      "description": "An example property of type number"
    },
    "amount": {
      "type": "integer",
      "title": "The amount of the ordered item",
      "description": "An example property of type integer"
    },
    "transportation": {
      "type": "string",
      "title": "The transportation method",
      "enum": [
        "standard",
        "express",
        "sameDay"
      ],
      "description": "An example property of type predefinedPrimitive"
    },
    "ingredients": {
      "type": "array",
      "title": "Ingredients",
      "items": {
        "enum": [
          "salt",
          "pepper",
          "cinnamon"
        ]
      },
      "uniqueItems": true,
      "description": "An example property of type subset"
    },
    "notificationMails": {
      "type": "array",
      "title": "Notification Mails",
      "description": "List of mail addresses to notify.",
      "items": {
        "type": "string"
      },
      "uniqueItems": true
    }
  },
  "required": [
    "id",
    "schema",
    "parent",
    "title"
  ]
}
`
