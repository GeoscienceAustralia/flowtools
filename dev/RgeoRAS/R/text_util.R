## Various text manipulation utilities


#################################################################################
#' Crude extension of 'grep' to take multiple patterns. 
#'
#' Count how many patterns in 'patterns' matches the text in 'text'
#' @param patterns A character vector of patterns
#' @param text the text to search
#' @return number of patterns that match the text. Note that this is cruder
#'          than 'grep', which returns the matching indices
#' @export
#' @examples
#'   # Use of vector grep
#'   mypatterns=c('Hi', 'J', 'Elephant')
#'   mytext="Hi there, I would like to meet your Elephant"
#'   print(vector_grep(mypatterns,mytext))
#'
vector_grep<-function(patterns, text){
    output=0
    for(i in 1:length(patterns)){
        tmp=grep(patterns[i], text)
        if(length(tmp)>0){
            output=output+1
        }
    }
    output
}

#' Format character strings to have a fixed length
#' 
#' Take a string, check its length, and if it is too short, then pad it to
#' the correct length. If it is too long, then truncate it. 
#'
#' @param mystring_vec character string vector
#' @param charlen Integer length to pad the string to
#' @param pad Character to pad the string with. Must be of length 1
#' @param justify 'left' or 'right' justify the string
#' @return A new character vector which is appropriately padded
#' @export
#' @examples
#'  x=pad_string('Mystring', 16, '-', 'left')
#'  print(x)
#'  stopifnot(x=='Mystring--------')
#'  x=pad_string('Mystring', 16, ' ', 'right')
#'  print(x)
#'  stopifnot(x=='        Mystring')
pad_string<-function(mystring_vec, charlen, pad=" ", justify='left'){

    if(nchar(pad)!=1) stop('nchar(pad) must equal 1')

    out=mystring_vec # Predefine
  
    # Loop over each vector element seperately 
    for(i in 1:length(mystring_vec)){ 
        mystring=mystring_vec[i]

        l=nchar(mystring) # Input string length

        if(l<=charlen){
            # String needs to be padded
            padtext=paste(rep(pad,charlen-l), collapse="")

            if(justify=='left'){
                out[i]=paste(mystring,padtext,sep="")
            }else if(justify=='right'){
                out[i]=paste(padtext,mystring,sep="")
            }

        }else if(l>charlen){
            # String needs to be truncated
            mystring_split=strsplit(mystring,"")[[1]] # Split to individual characters

            if(justify=='left'){
                out[i]=paste(mystring_split[1:charlen],sep="", collapse="")
            }else if(justify=='right'){
                out[i]=paste(mystring_split[(l-charlen+1):l], sep="", collapse="")
            }
        }
    }

    return(out)
}

##############################################################################################

#' Format in rows
#'
#' Function to re-format a character vector to have rows containing 'rowlen' entries
#' e.g for rowlen=2
#' c(' 01', ' 23', ' 34.5', 'my char', 'f') --> c(' 01 23', ' 34.5my char', 'f') 
#'
#' @param char_vec A character vector
#' @param rowlen The number of entries in each row
#' @return A character vector 
#' @export
#' @examples
#'   x=c(' 01', ' 23', ' 34.5', 'my char', 'f')
#'   y=format_in_rows(x, 2)
#'   stopifnot(y==c(' 01 23', ' 34.5my char', 'f'))
format_in_rows<-function(char_vec,rowlen){

    #browser()
    l=length(char_vec)
    #@ l1= Number of lines we need to add to make the number of next_lines elements a multiple of rowlen
    l1= (rowlen-l%%rowlen)%%rowlen 

    char_vec=c(char_vec, rep("", l1)) # Padded

    #@ Convert to matrix with rowlen columns
    char_vec2=matrix(char_vec,ncol=rowlen,byrow=T)
    
    #@ Convert to rows with rowlen entries
    #e.g. char_vec3=paste(char_vec2[,1], char_vec2[,2], char_vec2[,3], char_vec2[,4], char_vec2[,5], sep="")
    char_vec3=c()
    for(i in 1:rowlen){ 
        char_vec3=paste(char_vec3, char_vec2[,i],sep="") 
    }

    return(char_vec3)
}

#############################################################################

#' For reading structured text
#'
#' Take a string, split it into individual characters, and recombine them
#' into separate pieces of no_chars length. Then convert these to numeric,
#' and output as a vector
#'
#' @param string The character string to operate on
#' @param no_chars Integer: Split string into pieces with 'no_chars' characters 
#' @return A vector with the split string
#' @export
#' @examples
#'   x='abcdefghijkl'
#    y=split_nchars_numeric(x,4)
#    stopifnot(y==c('abcd', 'efgh', 'ijkl'))
split_nchars_numeric<-function(string,no_chars){

    tmp=unlist(strsplit(string, split=""))
    l=length(tmp)
    if(l%%no_chars!=0){
        print(c('tmp = ', tmp))
        print(c('no_chars = ', no_chars))
        print(c('Length tmp = ', l))
        stop('ERROR in split_nchars_numeric: Number of characters in string is not a multiple of no_chars')

    }

    l2=l/no_chars
    output=rep(NA,l2)

    for(i in 1:l2){
        output[i] = as.numeric(paste(tmp[(1:no_chars) + no_chars*(i-1)], sep="", collapse=""))
    }
    return(output)
}

###############################################################################
#
#' Write character vector containing hecras geometry to a file, in a way that works from both linux and windows machines.
#'
#' @param hec_lines character vector containing hecras geometry info
#' @param output_file output filename
#' @return Nothing, but saves the file
#' @export
heclines_2_File<-function(hec_lines, output_file){
        if(Sys.info()[1]=='Linux'){
            # Note the special carriage return, which suits windows (at least when
            # I run from linux -- test!
            cat(hec_lines,file=output_file,sep="\r\n")
        }else{
            # Good for Windows
            cat(hec_lines,file=output_file,sep="\n")
        }
        return(NULL)
}
