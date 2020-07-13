# Gauss - Jordan Method
# An algorithm that solves linear equations iteratively 


# 1. If the first entry in the initial row is 0, swap it with another row that has a non-zero entry in its first column. Otherwise, move to step 2.
# 2. Multiply through the current row by a scalar to make the leading entry equal to 1.
# 3. Add scaled multiples of the current row to every other row in the matrix until every entry in the current column, other than the leading 1 in the current row, is a 0.
# 4. Go back step 2 and repeat the process until the matrix is in reduced row-echelon form.

gauss_jordan <- function(matx){

    loops <- dim(matx)[1]
    rows <- loops
    
    # Check whether the leading entry is non-zero
    if(matx[1,1] == 0){
        for(r in 2:rows){
            if(matx[r,1] != 0){
                tmp <- matx[1,1]
                matx[1,1] <- mat[r,1]
                matx[r,1] <- tmp
            }
        }
    }
    
    for(i in 1:loops){
        leading_entry <- matx[i, i]
        # Transform the leading entry to 1
        matx[i, ] <- (1 / leading_entry) * matx[i, ]
        
        # Loop into each other entry in of the leading entry's column
        # and transform the other entries to zero
        for(k in 1:rows){
            # Skip the leading entry
            if(i == k){
                next
            }
            
            new_leading_entry <- matx[i,i]
            secondary <- matx[k,i]

            # Skip if the current entry is already 0
            if(secondary == 0){
                next
            }
            # Tranform the current entry to -
            else{
                # The coefficient that will make the non-leading entries equal to 0
                # new_leading_entry is always positive
                to_zero_coef <- -(secondary / new_leading_entry)
                
                secondary_row <- matx[k, ] 
                leading_row <- matx[i, ]
                # Operation
                
                matx[k, ] <- secondary_row + (to_zero_coef * leading_row)   
            }
            # print(matx)
            
        }
    }
    
    print("Gauss-Jordan Eliminatin Successful!")
    return(matx)
}

matx <- matrix(c(-1, -5, 1, 17,
         -1, -1, 1, 1,
         2, 5, -3, -10),
         nrow = 3,
         byrow = T)

solution_matrix <- gauss_jordan(matx)


soln_index <- dim(solution_matrix)[2]
vars <- paste0("X" , seq(1:dim(solution_matrix)[1]))

print("Solution Matrix:")
print(solution_matrix)
print("Solutions:")
          
for(i in seq_along(vars)){
 print(paste0(vars[i], ": ", solution_matrix[i, soln_index]))
       # print(i)
}
