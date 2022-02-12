# Init segment

Strips `uuid` boxes from DASH init segments.
This a script fix for content processed [REDACTED - with a "different configuration"]. In that case, additional information is included in the init segments and their size goes over what [REDACTED] supports.

## File format information

These are the most important sources of information I used to build this:

* https://www.cimarronsystems.com/wp-content/uploads/2017/04/Elements-of-the-H.264-VideoAAC-Audio-MP4-Movie-v2_0.pdf  
A step by step guide to opening an H.264 file. Init segments have fewer pieces, but it was useful nonetheless

* http://mp4ra.org/#/qtatom  
For a list of valid atom types

* http://atomicparsley.sourceforge.net/mpeg-4files.html  
Another step by step guide, this time to parsing MP4 files.

### The minimum to understand this code:

Content in an "ISO/IEC base media file format (ISOBMFF)" [Wikipedia](https://en.wikipedia.org/wiki/ISO/IEC_base_media_file_format) is structured in "atoms" or "boxes". Each box starts with 4 bytes for an integer indicating the amount of bytes to read to get the whole box, and 4 more bytes for a string that indicates the type of box. A box contains more boxes, or data (in a few cases, both things).  
&nbsp;  
All files start with an `ftyp` box, with some information about the type of file you are dealing with.  
In the case of this repository, we deal with DASH init segments. DASH init segments have an `ftyp` box, followed by an optional `free` box, and then a `moov` box. _(source for this missing, will add it if I find it again)_.  
In the `moov`, for the files processed with [REDACTED - different configuration], there's a last `uuid` box (custom metadata). 

This script drops those bytes, then goes back to the `moov` starting point and calculates the new byte size for box. Finally it writes the file again.

## Public API

The public API consists of several functions and a couple classes:  

### Classes

* `init-segment-file`: represents the whole file. It has a `boxes` attribute that in our case, always has the only two top-level boxes we deal with: `ftyp` and `moov`.
&nbsp;  
* `box/atom`: a base class for the other boxes, has the properties common to all of them: size, type and a list of child boxes. It also has a property `raw-data` that contains the byte array _for this the box and all of its child boxes_.
&nbsp;  
* `ftyp-box`: self-explanatory. Adds some relevant fields to validate things aren't broken when parsing a file we modified.
&nbsp;  
* `moov-box`: Since we don't go deep into the structure of this box, it doesn't have any extra fields.

### Functions

* `load-init-file`: generic function that takes a string, pathname or list of bytes, and returns the parsed output. The byte list version is very convenient for dev work :).  
&nbsp;  
* `remove-uuid-from-moov-box`: takes a local path and fixes the file. BUT by default it just returns a boolean that tells you if the file has an `uuid` box. With `:fix t` it will **overwrite** the file if it needs fixing.  
&nbsp;  
* `download-and-probe-files`: takes a list of files in the format returned by `boto3-wrapper:list-items`, and returns an [association list](https://lispcookbook.github.io/cl-cookbook/data-structures.html#alist) with the format `(path . has-uuid-box)`.  


### Package global variables

* `*local-dir*`: the local directory used to download the files from S3.
&nbsp;  
* `*bucket*`: the S3 bucket to use.

### Usage

I list all the `init.m4s` for the S3 directory using `boto3-wrapper`, then feed that to `download-and-probe-files` to identify files that need fixing.  
The lists in this directory with `.dump` extension were obtained in this way. [NOTE: for obvious reasons the dumps with lists of files corrected are not in the repository :)]
When ready, run the list of files doing `(in-seg:remove-uuid-from-moov-box the-path :fix t)`, and finally upload them all using boto3-wrapper again.  
Final step is to clear the CDN caches as needed.  

## Dependencies

If you clone this entire repo to `~/common-lisp`, and then clone `boto3-wrapper` to `~/quicklisp/local-projects`, it will all work.  
List of deps:
* Alexandria (utils), UIOP (utils and file operations)
* [boto3-wrapper](https://github.com/sebasmonia/boto3-wrapper) to access S3

