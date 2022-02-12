# bits-and-lisp

Small experiments to deal with binary data using Common Lisp.

## Project #1: read BMP files

My first idea was to open an easy image format. BMP is well documented (in MSDN and Wikipedia, among others) and relatively simple.  
I created a few BMPs with an image editor and worked my way from there. As of this writing, I am at a stage where I read all metadata fields and have the pixel array with the colors. There are other header versions that I could add to the code later, by checking out the specs and grabbing images from other sources.  
The file is read byte by byte per field, but then the pixel array is loaded in one giant chunck which obviously doesn't scale for big images.  
I plan to at some point revisit this and [use CL-GD](https://edicl.github.io/cl-gd) to write an output PNG of the source image pixel by pixel.  

## Project #2: manipulation of DASH init segments

A couple weeks after I started the BMP project, we had an issue at work where due to a misconfiguration, init segments were too big for a certain playback platform. To skip on a long (an expensive) re-process of the content, I thought I could try to script our way out of it.  
I am honestly quite proud of how this turned out given I wrote it in about 2.5 days. Of course the recent experience with `cl-bmp` meant some of the knowledge I needed was fresh. But I did have to learn quite a bit about DASH and [ISO Media Format](https://en.wikipedia.org/wiki/ISO/IEC_base_media_file_format) to make it all work.  
And _of course_ the scripts are shared here with permission and with some details ommitted :)  
The README in the `/init-segment` directory has more details about the info sources I used and the overall process.  


