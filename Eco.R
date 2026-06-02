my_strict_mean <- function(x, trim = 0, na.rm = FALSE, ...) {
 
  # 1. REJECT LISTS (excluding Data Frames)
  if (is.list(x) && !is.data.frame(x)) {
    stop("Input is a list. Lists are not supported; please provide a numeric vector, matrix, array, or data frame.")
  }
 
  # 2. ARRAY HANDLING
  # Column-wise mean inside each block/layer
  if (is.array(x) && !is.matrix(x)) {
   
    # Check valid array type
    if (!is.numeric(x) &&
        !is.logical(x) &&
        !is.complex(x)) {
     
      warning("array is not numeric, complex, or logical: returning NA")
      return(NA_real_)
    }
   
    # Number of blocks/layers
    num_blocks <- dim(x)[3]
   
    # Store block-wise results
    results <- lapply(1:num_blocks, function(i) {
     
      apply(x[,,i], 2, function(col) {
       
        my_strict_mean(col,
                       trim = trim,
                       na.rm = na.rm,
                       ...)
      })
    })
   
    # Name the blocks
    names(results) <- paste0("Block_", 1:num_blocks)
   
    return(results)
  }
 
  # 3. MATRIX HANDLING
  # Column-wise means
  if (is.matrix(x)) {
   
    # Check valid matrix type
    if (!is.numeric(x) &&
        !is.logical(x) &&
        !is.complex(x)) {
     
      warning("matrix is not numeric, complex, or logical: returning NA")
      return(NA_real_)
    }
   
    # Column-wise means
    results <- apply(x, 2, function(col) {
     
      my_strict_mean(col,
                     trim = trim,
                     na.rm = na.rm,
                     ...)
    })
   
    return(results)
  }
 
  # 4. DATA FRAME HANDLING
  if (is.data.frame(x)) {
   
    is_valid_col <- sapply(x, function(col)
      is.numeric(col) ||
        is.logical(col) ||
        is.complex(col))
   
    if (!any(is_valid_col)) {
      return("non-numeric")
    }
   
    results <- sapply(x, function(col) {
     
      if (is.numeric(col) ||
          is.logical(col) ||
          is.complex(col)) {
       
        # Recursive call
        return(my_strict_mean(col,
                              trim = trim,
                              na.rm = na.rm,
                              ...))
      } else {
        return(NA_real_)
      }
    })
   
    return(results)
  }
 
  # 5. TYPE CHECK
  if (!is.numeric(x) &&
      !is.complex(x) &&
      !is.logical(x)) {
   
    warning("argument is not numeric, complex, or logical: returning NA")
    return(NA_real_)
  }
 
  # 6. HANDLE MISSING VALUES
  if (isTRUE(na.rm)) {
    x <- x[!is.na(x)]
  }
 
  # 7. TRIM VALIDATION
  if (!is.numeric(trim) || length(trim) != 1L) {
    stop("'trim' must be numeric of length one")
  }
 
  n <- length(x)
 
  # 8. TRIM AND SPECIAL CASES
  if (trim > 0 && n > 0) {
   
    if (is.complex(x)) {
      stop("trimmed means are not defined for complex data")
    }
   
    if (anyNA(x)) {
      return(NA_real_)
    }
   
    if (trim >= 0.5) {
      return(stats::median(x, na.rm = FALSE))
    }
   
    lo <- floor(n * trim) + 1
    hi <- n + 1 - lo
   
    x <- sort.int(x,
                  partial = unique(c(lo, hi)))[lo:hi]
  }
 
  # 9. FINAL CALCULATION
  .Internal(mean(x))
}


