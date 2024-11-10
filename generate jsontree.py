import shutil
import json
import jsf
import randomfiletree

# https://github.com/json-schema-faker/json-schema-faker/blob/master/docs/USAGE.md#example-usage
SCHEMA = {
    "type": "object",
    "properties": {
        "user": {
            "type": "object",
            "properties": {
                "id": {
                    "$ref": "#/definitions/positiveInt",
                },
                "name": {
                    "type": "string",
                    "faker": "name.fullName",
                },
                "email": {
                    "type": "string",
                    "format": "email",
                    "faker": "internet.email",
                },
            },
            "required": ["id", "name", "email"],
        },
    },
    "required": ["user"],
    "definitions": {
        "positiveInt": {
            "type": "integer",
            "minimum": 0,
            "exclusiveMinimum": True,
        },
    },
}


def main():
    shutil.rmtree("json", ignore_errors=True)
    faker = jsf.JSF(SCHEMA)
    _, files = randomfiletree.iterative_gaussian_tree(
        "json",
        nfiles=5,
        nfolders=5,
        maxdepth=5,
        repeat=6,
    )
    for file in files:
        file.write_text(json.dumps(faker.generate()))


if __name__ == "__main__":
    main()
