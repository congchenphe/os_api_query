# Demo usage file

# source The query function file
source("./os_api_query.R")

# import the secret API key
KEY <-readLines("./keyfile")

# call get_address
result <- c(get_address("133","SE1 8UG", KEY,"",FALSE),
            get_address("133","SE18UG", KEY),
            get_address("Wellington House","SE1 8UG", KEY),
            get_address("Wellington House","SE18UG", KEY),
            get_address("10","SW1A 2AA", KEY),
            get_address("61","NW9 5DF", KEY),
            get_address("99","SO01 0ZZ", KEY), # should return a blank string
            get_address("99","SO01 0ZZ", KEY, "Nothing Found"),
            get_address("the meadows","SP5 1EZ", KEY, "Nothing Found"),
            get_address('999', 'AA1 9ZZ', KEY, "Zero Results")
            )


paste(result)

get_address_vectorized = Vectorize(get_address, vectorize.args = c('building', 'postcode'))

result1 = get_address_vectorized(building = c('133', '133', 'Wellington House', 'Wellington House', '10', '61', '99', '99', 'the meadows', '999'),
                                 postcode = c('SE1 8UG', 'SE18UG', 'SE1 8UG', 'SE18UG', 'SW1A2AA', 'NW9 5DF', 'SO01 0ZZ', 'SO01 0ZZ', 'SP5 1EZ', 'AA1 9ZZ'),
                                 api_key = KEY)


get_address_from_uprn_vectorized = Vectorize(get_address_from_uprn, vectorize.args = 'uprn_string')

get_address_from_uprn_vectorized(uprn_string = c('10033625525', '200010019924', 'fail_this'),
                                 api_key = KEY)
