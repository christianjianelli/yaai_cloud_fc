# yaai_cloud_fc
yaai_cloud_fc - ABAP AI Tools Cloud - Function Calling Library

## Purpose

This library provides tools designed to be used by AI agents created with [ABAP AI Tools Cloud](https://github.com/christianjianelli/yaai_cloud).

ABAP developers use ABAP AI Tools Cloud to create custom AI agents. These agents can be granted access to the tools in this library, allowing them to perform actions and manage ABAP objects in the cloud system.

## Tool Overview

The tools are organized by ABAP object type. Each tool exposes operations that agents can invoke to perform ABAP development tasks in the cloud system.

### Tool Categories

- **ABAP Class Tools**: Create and manage ABAP classes.
- **ABAP Interface Tools**: Create and manage ABAP interfaces.
- **DDIC Domain Tools**: Create, update, delete, read, and search domains.
- **DDIC Data Element Tools**: Create, update, delete, read, search, and translate.
- **DDIC Structure Tools**: Create, update, delete, read, and search structures.
- **DDIC Table Tools**: Create, update, delete, read, and search tables.
- **DDIC Table Type Tools**: Create, update, delete, read, and search table types.
- **Message Class Tools**: Create, update, delete, read, and translate.

## Documentation

See the `docs/` folder for detailed guides on each tool:
- [abap-class-tools.md](docs/abap-class-tools.md)
- [abap-interface-tools.md](docs/abap-interface-tools.md)
- [ddic-domain-tools.md](docs/ddic-domain-tools.md)
- [ddic-data-element-tools.md](docs/ddic-data-element-tools.md)
- [ddic-structure-tools.md](docs/ddic-structure-tools.md)
- [ddic-table-tools.md](docs/ddic-table-tools.md)
- [ddic-table-type-tools.md](docs/ddic-table-type-tools.md)
- [abap-message-class-tools.md](docs/abap-message-class-tools.md)

## Source Code

ABAP source code for each tool is located in the `src/` folder.

---
For further instructions and agent integration examples, refer to the documentation files in `docs/`.
