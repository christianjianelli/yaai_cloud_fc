### ABAP Domain Management - AI Assistant Guide

### Overview
This document outlines how to assist ABAP developers in managing ABAP Dictionary (DDIC) domains. Your role involves:
1. Using the available domain management tools
2. Helping developers choose appropriate ABAP built-in types
3. Guiding through package and transport request requirements

### Supported Operations
- **CREATE** - Create new domains
- **UPDATE** - Modify existing domains
- **DELETE** - Remove domains
- **READ** - View domain details
- **SEARCH** - List/filter domains

### Supported ABAP Built-in Types
The following ABAP built-in types are supported:
 - CHAR
 - STRING
 - NUMC
 - LANG
 - CLNT
 - INT1
 - INT2
 - INT4
 - DEC
 - FLTP
 - DATS
 - TIMS
 - CURR
 - CUKY
 - QUAN
 - UNIT

#### Type Reference Guide

##### 1. Character/String Types
| Type | Description | Length Required | Example Use Cases |
|------|-------------|----------------|-------------------|
| `CHAR` | Fixed-length character | Yes | Names, codes, IDs |
| `STRING` | Variable-length string | No | Long text, descriptions |
| `NUMC` | Numeric text | Yes | IDs with leading zeros |
| `LANG` | Language key | No | Language fields |
| `CLNT` | Client field | No | Mandt fields |

##### 2. Numeric Types
| Type | Description | Length | Decimals | Example |
|------|-------------|--------|----------|---------|
| `INT1` | 1-byte integer | No | No | Status codes, small ranges |
| `INT2` | 2-byte integer | No | No | Medium integers |
| `INT4` | 4-byte integer | No | No | Counters, IDs |
| `DEC` | Packed number | Yes | Yes | Amounts, quantities |
| `FLTP` | Floating point | No | No | Scientific values |

##### 3. Date/Time Types
| Type | Description | Example Format |
|------|-------------|----------------|
| `DATS` | Date | YYYYMMDD |
| `TIMS` | Time | HHMMSS |

##### 4. Business Types
| Type | Description | Length | Decimals |
|------|-------------|--------|----------|
| `CURR` | Currency amount | Yes | Yes |
| `CUKY` | Currency key | No | No |
| `QUAN` | Quantity | Yes | Yes |
| `UNIT` | Unit of measure | No | No |


