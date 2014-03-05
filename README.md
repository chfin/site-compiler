# site-compiler

A static site compiler from YAML to HTML.

## Introduction

Site-compiler uses cl-emb for templates and cl-yaclyaml for YAML parsing
to generate a set of HTML files from a set of YAML data files.
The data files serve as a simple NoSQL database, where each file is one document
containing a YAML map.
Each document has a schema which is associated with a cl-emb template
that is used for compiling the document.
During compilation the document is set as the template's environment,
so that its values can be referenced by their keys inside the template.

## Directories

Site-compiler operates on four directories,
each of which is controlled by one special variable:

* `*data-dir*` defines the source directory
* `*schema-dir*` defines the schema directory
* `*template-dir*` defines the schema directory
* `*site-dir*` defines the target directory

Each of theses variables should be an absolute pathname designator to a default file, 
eg. `"/home/chfin/data/default.yaml"`.
Especially the file type (.yaml) should be given,
since this will be the default file type for all files in this directory
and can be omitted in document ids (see [References](#references)).

## References

**TODO**

Documents can contain references to other files. This must be described in the schema files.

## Schemas

**TODO**

Schemas can have these keys:

* template
* includes
* keys
* link

Keys can have these attributes:

* foreign
* reverse
* link
* list
* index

## Usage

**TODO**

All exported symbols live in the `site-compiler` package.

To compile the complete data directory, run:
```common-lisp
SITE-COMPILER> (compile-all)
```

To compile a single file run
```common-lisp
SITE-COMPILER> (compile-yaml id)
```
where `id` is the documents id.

If you have reverse references, you have to create the index first:
```common-lisp
SITE-COMPILER> (create-index)
```

## LICENSE

MIT/X11, see LICENSE.
