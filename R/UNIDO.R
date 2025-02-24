#' UNIDO data
#'
#' Read and convert data from United Nations Industrial Organisation.
#'
#' @md
#' @param subtype one of
#'     - `INDSTAT2`: read INDSTAT 2 data
#'     - `INDSTAT3`: read INDSTAT 3 data from [https://stat.unido.org/data/download?dataset=indstat&revision=3]
#'     - `INDSTAT4`: read INDSTAT 4 data from [https://stat.unido.org/data/download?dataset=indstat&revision=4]
#'       INDSTAT 4 data quality has not been vetted and should not be used for
#'       production.
#' @param exclude Exclude faulty data (`TRUE`, default) or not.  Return a tibble
#'     of country/subsector/year combinations for `exclude = 'return'`.
#'     (Work-in-progress)
#' @param x result from `readUNIDO()` as passed to `convertUNIDO()`
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' `readUNIDO` returns raw INDSTAT data.  `convertUNIDO` converts to iso3c
#' country codes, selects industry subsectors value added data according to this
#' table
#'
#' | subsector     | ISIC | ctable | utable |
#' |---------------|------|--------|--------|
#' | manufacturing | D    | 20     | 17–20  |
#' | cement        | 26   | 20     | 17–20  |
#' | chemicals     | 24   | 20     | 17–20  |
#' | steel         | 27   | 20     | 17–20  |
#'
#' and filters data that is either unreasonable or would unduly bias regional
#' regressions according to this table
#'
#' | subsector     | iso3c | years            |
#' |---------------|-------|------------------|
#' | manufacturing | BIH   | 1990–91          |
#' | manufacturing | CHN   | 1963–97          |
#' | manufacturing | EGY   | 2018-19          |
#' | manufacturing | HKG   | 1963–2015        |
#' | manufacturing | MAC   | 1963–2015        |
#' | manufacturing | MDV   | 1963–2015        |
#' | cement        | BDI   | 1980–2010        |
#' | cement        | CIV   | 1990–93          |
#' | cement        | HKG   | 1973–79          |
#' | cement        | NAM   | 2007–10          |
#' | cement        | RUS   | 1970–90          |
#' | chemicals     | CIV   | 1989             |
#' | chemicals     | HKG   | 1973–79, 2008–15 |
#' | chemicals     | MAC   | 1978–79          |
#' | chemicals     | MMR   | 2021             |
#' | chemicals     | NER   | 1999–2002        |
#' | steel         | BGD   | 2011             |
#' | steel         | CHE   | 1995–96          |
#' | steel         | CHL   | 2008             |
#' | steel         | HKG   | 1973–79          |
#' | steel         | HRV   | 2012             |
#' | steel         | IRL   | 1980             |
#' | steel         | LKA   | 2006             |
#' | steel         | MAR   | 1989–2004        |
#' | steel         | MKD   | 1996             |
#' | steel         | PAK   | 1981–82          |
#' | steel         | TUN   | 2003–06          |
#' | all           | IRN   | 2022             |
#' | all           | IRQ   | 1992-2002        |
#' | all           | MWI   | 2021             |
#' | all           | TZA   | 2022             |
#'
#' `calcUNIDO()` calculates `otherInd` subsector values as the difference
#' between `manufacturing` and `cement`, `chemicals`, and `steel` values and is
#' intended to be called through [`calcOutput()`], which will aggregate regions.
#'
#' @author Michaja Pehl
#'
#' @seealso [`readSource()`], [`calcOutput()`]
#'
#' @importFrom assertr assert not_na verify
#' @importFrom dplyr anti_join between bind_rows filter group_by inner_join
#'     left_join mutate n select summarise ungroup
#' @importFrom GDPuc convertGDP
#' @importFrom madrat toolCountryFill
#' @importFrom magclass as.magpie
#' @importFrom magrittr %>%
#' @importFrom quitte list_to_data_frame madrat_mule
#' @importFrom readr col_character col_double col_integer col_skip cols read_csv
#' @importFrom rlang !!! .data is_empty syms
#' @importFrom tibble tibble tribble
#' @importFrom tidyr drop_na expand_grid unite

#' @rdname UNIDO
#' @export
readUNIDO <- function(subtype = 'INDSTAT3')
{
    base_path <- '~/PIK/swap/inputdata/sources/UNIDO/' # used for debugging
    base_path <- './'

    path <- switch(
        subtype,
        `INDSTAT2` = c(base_path, 'INDSTAT2', 'INDSTAT2_2017_ISIC_Rev_3.csv'),
        `INDSTAT3` = c(base_path, 'INDSTAT3_2025-01-09', 'data.csv'),
        `INDSTAT4` = c(base_path, 'INDSTAT4_2025-01-09', 'data.csv'),
        stop('subtype not implemented')) %>%
        paste(collapse = .Platform$file.sep)

    col_types <- switch(
        subtype,
        `INDSTAT2` = cols(ctable      = col_integer(),
                          country     = col_integer(),
                          year        = col_integer(),
                          isic        = col_character(),
                          isiccomb    = '-',
                          value       = col_double(),
                          utable      = col_integer(),
                          source      = '-',
                          lastupdated = col_double(),
                          unit        = '-'),
        `INDSTAT3` = cols(Year                       = col_integer(),
                          Country                    = '-',
                          CountryCode                = col_integer(),
                          Variable                   = '-',
                          VariableCode               = col_integer(),
                          UnconsolidatedVariableCode = col_integer(),
                          UnconsolidatedVariableName = '-',
                          ActivityCode               = col_character(),
                          Activity                   = '-',
                          ActivityCombination        = '-',
                          Value                      = '-',
                          UnitType                   = '-',
                          ValueUSD                   = col_double()),
        `INDSTAT4` = cols(Year                       = col_integer(),
                          Country                    = '-',
                          CountryCode                = col_integer(),
                          Variable                   = '-',
                          VariableCode               = col_integer(),
                          UnconsolidatedVariableCode = col_integer(),
                          UnconsolidatedVariableName = '-',
                          ActivityCode               = col_character(),
                          Activity                   = '-',
                          ActivityCombination        = '-',
                          Value                      = '-',
                          UnitType                   = '-',
                          ValueUSD                   = col_double()),
        stop('subtype not implemented'))

    col_names <- switch(subtype,
                        `INDSTAT2` = names(col_types$cols),
                        `INDSTAT3` = TRUE,
                        `INDSTAT4` = TRUE,
                        stop('subtype not implemented'))

    read_csv(file = path, col_names = col_names, col_types = col_types,
             na = '...') %>%
        madrat_mule() %>%
        return()
}

#' @rdname UNIDO
#' @export
convertUNIDO <- function(x, subtype = 'INDSTAT3', exclude = TRUE)
{
    harmonise_column_names <- function(d, subtype)
    {
        d %>%
            select(
                switch(subtype,
                       `INDSTAT2` = c('country', 'year', 'isic', 'ctable',
                                      'utable', 'lastupdated', 'value'),
                       `INDSTAT3` = c('country' = 'CountryCode',
                                      'year'    = 'Year',
                                      'isic'    = 'ActivityCode',
                                      'ctable'  = 'VariableCode',
                                      'utable'  = 'UnconsolidatedVariableCode',
                                      'value'   = 'ValueUSD'),
                       `INDSTAT4` = c('country' = 'CountryCode',
                                      'year'    = 'Year',
                                      'isic'    = 'ActivityCode',
                                      'ctable'  = 'VariableCode',
                                      'utable'  = 'UnconsolidatedVariableCode',
                                      'value'   = 'ValueUSD'),
                       stop('unknown subtype')))
    }

    add_iso3c <- function(d, subtype)
    {
        # data ----
        ## countries to remove ----
        # SUN data synthetic anyhow
        countries_to_remove <- 810

        ## countries to substitute ----
        # FIXME We are substituting some historic country codes by 'default'
        # codes of current countries.  Generally, they are situated in the same
        # aggregation region, so this has no impact on the derivation of
        # regional statistics.  This does not apply to former Yugoslavia
        # however.  Since the countries in question (currently Slovenia and
        # Croatia, others might join the EU at a later time and then require
        # reclassification) are small compared to the respective regions
        # (Europe and Rest-of-World), the impact should be limited.
        countries_to_substitute <- switch(
            subtype,

            `INDSTAT2` = tribble(
                ~country,   ~replacement,
                200,        203,            # CSE for CSK
                530,        531,            # CUW for ANT
                890,        688,            # SRB for YUG
                891,        688             # SRB for SCG
            ),

            `INDSTAT3` = tribble(
                ~country,   ~replacement,
                200,        203,            # CSE for CSK
                412,        688,            # SRB for Kosovo
                530,        531,            # CUW for ANT
                890,        688,            # SRB for YUG
                891,        688             # SRB for SCG
            ),

            `INDSTAT4` = tribble(
                ~country,   ~replacement,
                200,        203,            # CSE for CSK
                412,        688,            # SRB for Kosovo
                530,        531,            # CUW for ANT
                890,        688,            # SRB for YUG
                891,        688             # SRB for SCG
            ),

            stop('unknown subtype'))

        ## additional country codes ----
        # which are missing from the countrycode package
        additional_country_codes <- tribble(
            ~iso3c,   ~un,
            'TWN',    158,   # Republic of China
            'ETH',    230,   # Ethiopia and Eritrea
            'DEU',    278,   # East Germany
            'DEU',    280,   # West Germany
            'PAN',    590,   # Panama
            'SDN',    736    # Sudan
        )

        # process ----
        d %>%
            # exclude countries
            filter(!.data$country %in% countries_to_remove) %>%
            # substitute countries
            left_join(countries_to_substitute, 'country') %>%
            mutate(country = ifelse(!is.na(.data$replacement),
                                    .data$replacement,
                                    .data$country)) %>%
            select(-'replacement') %>%
            # add iso3c codes
            left_join(
                bind_rows(
                    countrycode::codelist %>%
                        select('iso3c', 'un') %>%
                        filter(!is.na(.data$iso3c), !is.na(.data$un)),

                    additional_country_codes
                ),

                c('country' = 'un')
            ) %>%
            select(-'country') %>%
            assert(not_na, everything())

    }

    most_recent_value <- function(d)
    {
        if ('lastupdated' %in% colnames(d)) {
            d <- d %>%
                group_by(!!!syms(setdiff(colnames(d),
                                         c('lastupdated', 'value')))) %>%
                filter(max(.data$lastupdated) == .data$lastupdated) %>%
                ungroup() %>%
                select(-'lastupdated')
        }
        return(d)
    }

    drop_duplicates <- function(d)
    {
        d %>%
            # filter duplicates, which stem from split countries (e.g. CUW)
            group_by(!!!syms(setdiff(colnames(d), 'value'))) %>%
            summarise(value = max(.data$value), .groups = 'drop')
    }

    select_subsectors <- function(d)
    {
        d %>%
            inner_join(
                tribble(
                    ~subsector,        ~isic,
                    'manufacturing',   'D',
                    'cement',          '26',
                    'chemicals',       '24',
                    'steel',           '27') %>%
                    expand_grid(ctable = 20, utable = 17:20),

                c('isic', 'ctable', 'utable')
            ) %>%
            select(-'isic', -'ctable', -'utable')
    }

    to_exclude <- function(d, subtype)
    {
        switch(
            subtype,
            bind_rows(   # default for all subtypes for now
                list_to_data_frame(
                    list(
                        # unreasonable data
                        EGY = 2018:2019,
                        IRN = 2022,
                        IRQ = 1992:2002,
                        MDV = unique(d$year),
                        MWI = 2021,
                        BIH = 1990:1991,
                        TZA = 2022,
                        # unrepresentative data
                        HKG = unique(d$year),
                        MAC = unique(d$year),
                        CHN = min(d$year):1997
                    ),
                    'iso3c', 'year'
                ) %>%
                    mutate(subsector = 'manufacturing'),

                # Data with an obvious mismatch between steel production and
                # steel value added figures is excluded.
                # Data for Hong Kong (1973-1979) is excluded since no data for
                # PR China is available for this period and the data would bias
                # any regression for the CHA region.
                list_to_data_frame(
                    list(BGD = 2011,
                         CHE = 1995:1996,
                         CHL = 2008,
                         HKG = 1973:1979,
                         HRV = 2012,
                         IRL = 1980,
                         IRN = 2022,
                         IRQ = c(1992, 1993, 2001, 2002, 2011, 2014),
                         LKA = 2006,
                         MAR = 1989:2004,
                         MKD = 1996,
                         MWI = 2021,
                         PAK = 1981:1982,
                         TUN = 2003:2006,
                         TZA = 2022
                    ),
                    'iso3c', 'year'
                ) %>%
                    mutate(subsector = 'steel'),

                list_to_data_frame(
                    list(BDI = 1980:2010,   # zero cement production
                         CIV = 1990:1993,   # cement VA 100 times higher than
                                            # before and after
                         NAM = 2007:2010,   # zero cement production
                         HKG = 1973:1979,   # no data for CHN prior to 1980
                         IRN = 2022,
                         IRQ = 1992:2002,   # cement VA 100 times higher than
                                            # before and after
                         MWI = 2021,
                         RUS = 1970:1990,   # exclude data from Soviet period,
                                            # which biases projections up
                         TZA = 2022
                    ),
                    'iso3c', 'year'
                ) %>%
                    mutate(subsector = 'cement'),

                list_to_data_frame(
                    list(CIV = 1989,
                         IRN = 2022,
                         IRQ = 1994:1997,
                         MMR = 2021,
                         MWI = 2021,
                         NER = 1999:2002,
                         HKG = c(1973:1979, 2008:2015),
                         MAC = c(1978:1979),
                         TZA = 2022
                    ),
                    'iso3c', 'year'
                ) %>%
                    mutate(subsector = 'chemicals')
            )
        )
    }

    exclude_subsectors <- function(d, subtype, exclude = TRUE)
    {

        if (isTRUE(exclude)) {
            d <- d %>%
                anti_join(
                    to_exclude(d, subtype),

                    c('iso3c', 'year', 'subsector')
                )
        }
        return(d)
    }

    convert_dollarbucks <- function(d, subtype)
    {
        d %>%
            GDPuc::toolConvertGDP(
                unit_in = switch(subtype,
                                 `INDSTAT2` = 'constant 2005 US$MER',
                                 `INDSTAT3` = 'constant 2005 US$MER', # ???
                                 `INDSTAT4` = 'constant 2005 US$MER', # ???
                                 stop('unknown subtype')),
                unit_out = mrdrivers::toolGetUnitDollar(),
                replace_NAs = 'with_USA')
    }

    # process data ----
    if ('return' == exclude)
        return(to_exclude(madrat_mule(x), subtype))

    x %>%
        madrat_mule() %>%
        harmonise_column_names(subtype) %>%
        drop_na('value') %>%
        select_subsectors() %>%
        add_iso3c(subtype) %>%
        exclude_subsectors(subtype, isTRUE(exclude)) %>%
        convert_dollarbucks(subtype) %>%
        most_recent_value() %>%
        drop_duplicates() %>%
        select('iso3c', 'year', 'subsector', 'value') %>%
        as.magpie(spatial = 1, temporal = 2, tidy = TRUE) %>%
        toolCountryFill(verbosity = 2) %>%
        return()
}

#' @rdname UNIDO
#' @export
calcUNIDO <- function(subtype = 'INDSTAT3')
{
    known_subtypes <- c('INDSTAT2', 'INDSTAT3', 'INDSTAT4')
    if (subtype %in% known_subtypes) {
        x <- readSource(type = 'UNIDO', subtype = subtype, convert = TRUE)

        x_manufacturing <- dimSums(x[,,'manufacturing'], dim = 3)
        x_no_manufacturing <- x[,,'manufacturing', invert = TRUE]
        x_otherInd <- setNames(
            ( x_manufacturing
            - dimSums(x_no_manufacturing, dim = 3)
            ),
            'otherInd')

        # $/year * 1e-9 $bn/$ = $bn/year
        x <- mbind(x_no_manufacturing, x_otherInd) * 1e-9

        # give proper variable names
        subsector_names <- c('cement', 'chemicals', 'steel', 'otherInd')
        variable_names  <- paste0('Value Added|Industry|',
                                  c('Cement', 'Chemicals', 'Steel',
                                    'Other Industry'))

        getNames(x) <- variable_names[match(getNames(x), subsector_names)]

        return(list(x = x,
                    weight = NULL,
                    unit = 'billion US$2017/yr',
                    description = 'industry subsector value added'))
    }
    else {
        stop(paste('Invalid subtype.  Supported subtypes are:',
                   paste(known_subtypes, collapse = ', ')))
    }
}
