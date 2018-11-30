package json

import (
	"errors"
	"github.com/santhosh-tekuri/jsonschema"
	"io"
	"path"
	"strings"
)

type PropertyInfo struct {
	Visible  bool
	Editable bool
	Type     string
}

// Read properties information of a type specified by the given schemaReader.
// The schema has to be valid against the meta schema.
// The types have to be defined in definitions.
func ReadPropertyInfo(schemaReader io.Reader) (map[string]PropertyInfo, error) {
	compiler := jsonschema.NewCompiler()
	if err := compiler.AddResource("meta.json", strings.NewReader(meta)); err != nil {
		return nil, err
	} else if err = compiler.AddResource("http://verinice.com/veo/draft-01/definitions.json", strings.NewReader(definitions)); err != nil {
		return nil, err
	} else if metaSchema, err := compiler.Compile("meta.json"); err != nil {
		return nil, err
	} else if schemaObj, err := jsonschema.DecodeJSON(schemaReader); err != nil {
		return nil, err
	} else if err = metaSchema.ValidateInterface(schemaObj); err != nil {
		return nil, err
	} else if schemaMap, ok := schemaObj.(map[string]interface{}); !ok {
		return nil, err
	} else if properties, ok := schemaMap["properties"].(map[string]interface{}); !ok {
		return nil, errors.New("JSON schema has no properties defined")
	} else {
		infoMap := make(map[string]PropertyInfo)
		for propertyName, propertySchema := range properties {
			if info, err := propertyInfo(metaSchema, propertySchema); err != nil {
				return nil, err
			} else {
				switch propertyName {
				case "$veo.id", "$veo.type", "$veo.title":
					info.Visible = false
					info.Editable = false
				}
				infoMap[propertyName] = *info
			}
		}
		return infoMap, nil
	}
}

func propertyInfo(metaSchema *jsonschema.Schema, propertySchema interface{}) (*PropertyInfo, error) {
	// we could traverse through all entries in 'definitions' and validate against
	// containing schemas, but we don't know which entries are schemas, hence we traverse
	// throw the 'oneOf' entry in the meta schema, since such referenced elements have to be
	// proper json schemas, otherwise the jsonschema.Compiler would have reported an error.
	if len(metaSchema.Properties["properties"].PatternProperties) != 1 {
		return nil, errors.New("the meta schema shall define exactly one pattern property for type properties")
	}
	for _, patternPropertySchema := range metaSchema.Properties["properties"].PatternProperties {
		for _, schema := range patternPropertySchema.OneOf {
			if schema.Ref.ValidateInterface(propertySchema) == nil {
				return &PropertyInfo{Visible: true, Editable: true, Type: path.Base(schema.Ref.Ptr)}, nil
			}
		}
	}
	return nil, nil
}

const meta = `
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "id": "http://verinice.com/veo/draft-01/schema#",
  "title": "veo element schema meta-schema",
  "type": [
    "object"
  ],
  "properties": {
    "$id": {
      "type": "string",
      "format": "uri-reference"
    },
    "$schema": {
      "type": "string",
      "format": "uri"
    },
    "$comment": {
      "type": "string"
    },
    "title": {
      "type": "string"
    },
    "description": {
      "type": "string"
    },
    "definitions": {
      "type": "object",
      "additionalProperties": {
        "$ref": "#"
      },
      "default": {}
    },
    "properties":
        {
          "description": "Each schema needs a set of properties. For a property schema to be valid it has to be one of the predefined property schema, i.e type, id, title or an arbitrary pattern property describing a valid type, e.g. boolean, dateTime etc. Note that this object is a schema describing the property named 'properties'",
          "properties": {
            "$veo.type": {
              "$ref": "http://verinice.com/veo/draft-01/definitions.json#/predefined"
            },
            "$veo.title": {
              "$ref": "http://verinice.com/veo/draft-01/definitions.json#/string"
            },
            "$veo.id": {
              "$ref": "http://verinice.com/veo/draft-01/definitions.json#/string"
            }
          },
          "patternProperties": {
            "^[a-z][a-zA-Z0-9]*$": {
              "description": "Define a distinct set of types allowed for element schemas. Distinction allows treating properties as strictly typed.",
              "oneOf": [
                {
                  "$ref": "http://verinice.com/veo/draft-01/definitions.json#/string"
                },
                {
                  "$ref": "http://verinice.com/veo/draft-01/definitions.json#/boolean"
                },
                {
                  "$ref": "http://verinice.com/veo/draft-01/definitions.json#/integer"
                },
                {
                  "$ref": "http://verinice.com/veo/draft-01/definitions.json#/number"
                },
                {
                  "$ref": "http://verinice.com/veo/draft-01/definitions.json#/dateTime"
                },
                {
                  "$ref": "http://verinice.com/veo/draft-01/definitions.json#/predefined"
                },
                {
                  "$ref": "http://verinice.com/veo/draft-01/definitions.json#/set"
                },
                {
                  "$ref": "http://verinice.com/veo/draft-01/definitions.json#/subset"
                }
              ]
            }
          },
          "additionalProperties": false,
          "required": [
            "$veo.type",
            "$veo.id",
            "$veo.title"
          ]
        }
  },
  "required": [
    "title",
    "type",
    "properties"
  ],
  "default": true
}
`

const definitions = `{
  "string": {
    "type": "object",
    "properties": {
      "type": {
        "enum": [
          "string"
        ]
      },
      "title": {
        "type": "string"
      },
      "description": {
        "type": "string"
      },
      "examples": {
        "type": "array"
      }
    },
    "required": [
      "type",
      "title"
    ],
    "additionalProperties": false
  },
  "boolean": {
    "type": "object",
    "properties": {
      "type": {
        "enum": [
          "boolean"
        ]
      },
      "title": {
        "type": "string"
      },
      "description": {
        "type": "string"
      }
    },
    "required": [
      "type",
      "title"
    ],
    "additionalProperties": false
  },
  "number": {
    "type": "object",
    "properties": {
      "type": {
        "enum": [
          "number"
        ]
      },
      "title": {
        "type": "string"
      },
      "description": {
        "type": "string"
      }
    },
    "required": [
      "type",
      "title"
    ],
    "additionalProperties": false
  },
  "integer": {
    "type": "object",
    "properties": {
      "type": {
        "enum": [
          "integer"
        ]
      },
      "title": {
        "type": "string"
      },
      "description": {
        "type": "string"
      }
    },
    "required": [
      "type",
      "title"
    ],
    "additionalProperties": false
  },
  "dateTime": {
    "type": "object",
    "properties": {
      "type": {
        "enum": [
          "string"
        ]
      },
      "title": {
        "type": "string"
      },
      "description": {
        "type": "string"
      },
      "format": {
        "enum": [
          "date-time"
        ]
      },
      "examples": {
        "type": "array"
      }
    },
    "required": [
      "type",
      "title",
      "format"
    ]
  },
  "predefined": {
    "description": "A predefined property is a property for which only a predefined primitive value set is valid, similar to an enum.",
    "oneOf": [
      {
        "$ref": "#/predefined-string"
      },
      {
        "$ref": "#/predefined-integer"
      },
      {
        "$ref": "#/predefined-number"
      }
    ]
  },
  "predefined-string": {
    "properties": {
      "type": {
        "enum": [
          "string"
        ]
      },
      "title": {
        "type": "string"
      },
      "enum": {
        "type": "array",
        "items": {
          "type": "string"
        }
      },
      "examples": {
        "type": "array"
      }
    },
    "required": [
      "enum"
    ]
  },
  "predefined-number": {
    "properties": {
      "type": {
        "enum": [
          "number"
        ]
      },
      "title": {
        "type": "string"
      },
      "enum": {
        "type": "array",
        "items": {
          "type": "number"
        }
      },
      "examples": {
        "type": "array"
      }
    },
    "required": [
      "enum"
    ]
  },
  "predefined-integer": {
    "properties": {
      "type": {
        "enum": [
          "integer"
        ]
      },
      "title": {
        "type": "string"
      },
      "enum": {
        "type": "array",
        "items": {
          "type": "integer"
        }
      },
      "examples": {
        "type": "array"
      }
    },
    "required": [
      "enum",
      "type"
    ]
  },
  "set": {
    "type": "object",
    "description": "A set is an array but can only contain unique primitive values.",
    "properties": {
      "type": {
        "enum": [
          "array"
        ]
      },
      "title": {
        "type": "string"
      },
      "description": {
        "type": "string"
      },
      "items": {
        "enum": [
          {
            "type": "string"
          },
          {
            "type": "integer"
          },
          {
            "type": "number"
          }
        ]
      },
      "uniqueItems": {
        "enum": [
          true
        ]
      },
      "examples": {
        "type": "array"
      }
    },
    "required": [
      "type",
      "title",
      "items",
      "uniqueItems"
    ]
  },
  "subset": {
    "type": "object",
    "description": "A subset is an array but can only contain unique values of a predefined set.",
    "properties": {
      "type": {
        "enum": [
          "array"
        ]
      },
      "title": {
        "type": "string"
      },
      "description": {
        "type": "string"
      },
      "items": {
        "$ref": "#/predefined"
      },
      "uniqueItems": {
        "enum": [
          true
        ]
      },
      "examples": {
        "type": "array"
      }
    },
    "required": [
      "type",
      "title",
      "items",
      "uniqueItems"
    ]
  }
}
`
