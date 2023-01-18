shortest :: [[a]] -> [a]
shortest [] = [] -- returns empty list is it is empty
shortest [a] = a -- if list contains one item return it 
shortest (first:second) = if length first < length(shortest second)
    then first
    else shortest second