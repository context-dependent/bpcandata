#' Download LFS PUMF data from StatCan website
#' @param years A vector of years to download
#' @param cache_dir The directory to cache the downloaded files
#' @param refresh_cache
#'  Whether to force download even if the files are already cached
#' @return Invisible
#' @export
fetch_lfs_pumf <- function(
    years, cache_dir = here::here(),
    refresh_cache = FALSE) {
    years |>
        purrr::walk(
            .fetch_single_year,
            cache_dir = cache_dir,
            refresh_cache = refresh_cache
        )
}

#' @rdname fetch_lfs_pumf
.fetch_single_year <- function(
    year,
    cache_dir,
    refresh_cache = FALSE) {
    if (!refresh_cache && fs::dir_exists(glue::glue("{cache_dir}/{year}"))) {
        message(
            glue::glue(
                "Skipping {year} as it is already cached,",
                " set refresh_cache = TRUE to force download."
            )
        )
        return(invisible())
    }

    u <- glue::glue(
        "https://www150.statcan.gc.ca/",
        "n1/pub/71m0001x/2021001/hist/{year}-CSV.zip"
    )
    zip_path <- glue::glue("{cache_dir}/{year}-CSV.zip")
    csv_dir <- glue::glue("{cache_dir}/{year}")

    if (!fs::dir_exists(csv_dir)) fs::dir_create(csv_dir)

    curl::curl_fetch_disk(u, zip_path)
    unzip(zip_path, exdir = csv_dir)
    fs::file_delete(zip_path)
    invisible()
}

#' Read LFS PUMF
#' @param dir The directory containing the LFS PUMF (e.g. data/raw/2020)
#' @return A list with two elements: records and codebook
#' Codebook: \itemize{
#'   \item{\code{REC_NUM} \code{<num>} Order of record in file [1, 99175] \code{for} All respondents}
#'   \item{\code{SURVYEAR} \code{<num>} Survey year [2020, 2020] \code{for} All respondents}
#'   \item{\code{SURVMNTH} \code{<fct>} Survey month \code{for} All respondents}{
#'     \itemize{
#'       \item{\code{1} January}
#'       \item{\code{2} February}
#'       \item{\code{3} March}
#'       \item{\code{4} April}
#'       \item{\code{5} May}
#'       \item{\code{6} June}
#'       \item{\code{7} July}
#'       \item{\code{8} August}
#'       \item{\code{9} September}
#'       \item{\code{10} October}
#'       \item{\code{11} November}
#'       \item{\code{12} December}
#'     }}
#'   \item{\code{LFSSTAT} \code{<fct>} Labour force status \code{for} All respondents}{
#'     \itemize{
#'       \item{\code{1} Employed, at work}
#'       \item{\code{2} Employed, absent from work}
#'       \item{\code{3} Unemployed}
#'       \item{\code{4} Not in labour force}
#'     }}
#'   \item{\code{PROV} \code{<fct>} Province \code{for} All respondents}{
#'     \itemize{
#'       \item{\code{10} Newfoundland and Labrador}
#'       \item{\code{11} Prince Edward Island}
#'       \item{\code{12} Nova Scotia}
#'       \item{\code{13} New Brunswick}
#'       \item{\code{24} Quebec}
#'       \item{\code{35} Ontario}
#'       \item{\code{46} Manitoba}
#'       \item{\code{47} Saskatchewan}
#'       \item{\code{48} Alberta}
#'       \item{\code{59} British Columbia}
#'     }}
#'   \item{\code{CMA} \code{<fct>} Nine largest CMAs \code{for} All respondents}{
#'     \itemize{
#'       \item{\code{1} Qu�bec}
#'       \item{\code{2} Montr�al}
#'       \item{\code{3} Ottawa�Gatineau (Ontario part)}
#'       \item{\code{4} Toronto}
#'       \item{\code{5} Hamilton}
#'       \item{\code{6} Winnipeg}
#'       \item{\code{7} Calgary}
#'       \item{\code{8} Edmonton}
#'       \item{\code{9} Vancouver}
#'       \item{\code{0} Other CMA or non-CMA}
#'     }}
#'   \item{\code{AGE_12} \code{<fct>} Five-year age group of respondent \code{for} All respondents}{
#'     \itemize{
#'       \item{\code{1} 15 to 19 years}
#'       \item{\code{2} 20 to 24 years}
#'       \item{\code{3} 25 to 29 years}
#'       \item{\code{4} 30 to 34 years}
#'       \item{\code{5} 35 to 39 years}
#'       \item{\code{6} 40 to 44 years}
#'       \item{\code{7} 45 to 49 years}
#'       \item{\code{8} 50 to 54 years}
#'       \item{\code{9} 55 to 59 years}
#'       \item{\code{10} 60 to 64 years}
#'       \item{\code{11} 65 to 69 years}
#'       \item{\code{12} 70 and over}
#'     }}
#'   \item{\code{AGE_6} \code{<fct>} Age in 2 and 3 year groups, 15 to 29 \code{for} Respondents aged 15 to 29 years}{
#'     \itemize{
#'       \item{\code{1} 15 to 16 years}
#'       \item{\code{2} 17 to 19 years}
#'       \item{\code{3} 20 to 21 years}
#'       \item{\code{4} 22 to 24 years}
#'       \item{\code{5} 25 to 26 years}
#'       \item{\code{6} 27 to 29 years}
#'     }}
#'   \item{\code{SEX} \code{<fct>} Sex of respondent \code{for} All respondents}{
#'     \itemize{
#'       \item{\code{1} Male}
#'       \item{\code{2} Female}
#'     }}
#'   \item{\code{MARSTAT} \code{<fct>} Marital status of respondent \code{for} All respondents}{
#'     \itemize{
#'       \item{\code{1} Married}
#'       \item{\code{2} Living in common-law}
#'       \item{\code{3} Widowed}
#'       \item{\code{4} Separated}
#'       \item{\code{5} Divorced}
#'       \item{\code{6} Single, never married}
#'     }}
#'   \item{\code{EDUC} \code{<fct>} Highest educational attainment \code{for} All respondents}{
#'     \itemize{
#'       \item{\code{0} 0 to 8 years}
#'       \item{\code{1} Some high school}
#'       \item{\code{2} High school graduate}
#'       \item{\code{3} Some postsecondary}
#'       \item{\code{4} Postsecondary certificate or diploma}
#'       \item{\code{5} Bachelor's degree}
#'       \item{\code{6} Above bachelor's degree}
#'     }}
#'   \item{\code{MJH} \code{<fct>} Single or multiple jobholder \code{for} Currently employed only}{
#'     \itemize{
#'       \item{\code{1} Single jobholder, including job changers}
#'       \item{\code{2} Multiple jobholder}
#'     }}
#'   \item{\code{EVERWORK} \code{<fct>} Identifies if a person has worked in the last year \code{for} Not currently employed}{
#'     \itemize{
#'       \item{\code{1} Yes, within last year}
#'       \item{\code{2} Yes, more than 1 year ago}
#'       \item{\code{3} No, never worked}
#'     }}
#'   \item{\code{FTPTLAST} \code{<fct>} Full- or part-time status of last job \code{for} Not currently employed but worked within the previous twelve months}{
#'     \itemize{
#'       \item{\code{1} Full-time (30 hours or more)}
#'       \item{\code{2} Part-time (1 to 29 hours)}
#'     }}
#'   \item{\code{COWMAIN} \code{<fct>} Class of worker, main job \code{for} Currently employed or worked within the past 12 months}{
#'     \itemize{
#'       \item{\code{1} Public sector employees}
#'       \item{\code{2} Private sector employees}
#'       \item{\code{3} Self-employed incorporated, with paid help}
#'       \item{\code{4} Self-employed incorporated, no paid help}
#'       \item{\code{5} Self-employed unincorporated, with paid help}
#'       \item{\code{6} Self-employed unincorporated, no paid help}
#'       \item{\code{7} Unpaid family worker}
#'     }}
#'   \item{\code{IMMIG} \code{<fct>} Immigrant status \code{for} All respondents}{
#'     \itemize{
#'       \item{\code{1} Immigrant, landed 10 or less years earlier}
#'       \item{\code{2} Immigrant, landed more than 10 years earlier}
#'       \item{\code{3} Non-immigrant}
#'     }}
#'   \item{\code{NAICS_21} \code{<fct>} Industry of main job \code{for} Currently employed or worked within the past 12 months}{
#'     \itemize{
#'       \item{\code{1} Agriculture}
#'       \item{\code{2} Forestry and logging and support activities for forestry}
#'       \item{\code{3} Fishing, hunting and trapping}
#'       \item{\code{4} Mining, quarrying, and oil and gas extraction}
#'       \item{\code{5} Utilities}
#'       \item{\code{6} Construction}
#'       \item{\code{7} Manufacturing - durable goods}
#'       \item{\code{8} Manufacturing - non-durable goods}
#'       \item{\code{9} Wholesale trade}
#'       \item{\code{10} Retail trade}
#'       \item{\code{11} Transportation and warehousing}
#'       \item{\code{12} Finance and insurance}
#'       \item{\code{13} Real estate and rental and leasing}
#'       \item{\code{14} Professional, scientific and technical services}
#'       \item{\code{15} Business, building and other support services}
#'       \item{\code{16} Educational services}
#'       \item{\code{17} Health care and social assistance}
#'       \item{\code{18} Information, culture and recreation}
#'       \item{\code{19} Accommodation and food services}
#'       \item{\code{20} Other services (except public administration)}
#'       \item{\code{21} Public administration}
#'     }}
#'   \item{\code{NOC_10} \code{<fct>} Occupation at main job \code{for} Currently employed or worked within the past 12 months}{
#'     \itemize{
#'       \item{\code{1} Management occupations}
#'       \item{\code{2} Business, finance and administration occupations, except management}
#'       \item{\code{3} Natural and applied sciences and related occupations, except management}
#'       \item{\code{4} Health occupations, except management}
#'       \item{\code{5} Occupations in education, law and social, community and government services, except management}
#'       \item{\code{6} Occupations in art, culture, recreation and sport, except management}
#'       \item{\code{7} Sales and service occupations, except management}
#'       \item{\code{8} Trades, transport and equipment operators and related occupations, except management}
#'       \item{\code{9} Natural resources, agriculture and related production occupations, except management}
#'       \item{\code{10} Occupations in manufacturing and utilities, except management}
#'     }}
#'   \item{\code{NOC_43} \code{<fct>} Occupation at main job \code{for} Currently employed or worked within the past 12 months}{
#'     \itemize{
#'       \item{\code{1} Legislative and senior management occupations}
#'       \item{\code{2} Specialized middle management occupations}
#'       \item{\code{3} Middle management occupations in retail and wholesale trade and customer services}
#'       \item{\code{4} Middle management occupations in trades, transportation, production and utilities}
#'       \item{\code{5} Professional occupations in finance}
#'       \item{\code{6} Professional occupations in business}
#'       \item{\code{7} Administrative and financial supervisors and specialized administrative occupations}
#'       \item{\code{8} Administrative occupations and transportation logistics occupations}
#'       \item{\code{9} Administrative and financial support and supply chain logistics occupations}
#'       \item{\code{10} Professional occupations in natural sciences}
#'       \item{\code{11} Professional occupations in applied sciences (except engineering)}
#'       \item{\code{12} Professional occupations in engineering}
#'       \item{\code{13} Technical occupations related to natural and applied sciences}
#'       \item{\code{14} Health treating and consultation services professionals}
#'       \item{\code{15} Therapy and assessment professionals}
#'       \item{\code{16} Nursing and allied health professionals}
#'       \item{\code{17} Technical occupations in health}
#'       \item{\code{18} Assisting occupations in support of health services}
#'       \item{\code{19} Professional occupations in law}
#'       \item{\code{20} Professional occupations in education services}
#'       \item{\code{21} Professional occupations in social and community services}
#'       \item{\code{22} Professional occupations in government services}
#'       \item{\code{23} Occupations in front-line public protection services}
#'       \item{\code{24} Paraprofessional occupations in legal, social, community and education services}
#'       \item{\code{25} Assisting occupations in education and in legal and public protection}
#'       \item{\code{26} Care providers and public protection support occupations and student monitors, crossing guards and related occupations}
#'       \item{\code{27} Professional occupations in art and culture}
#'       \item{\code{28} Technical occupations in art, culture and sport}
#'       \item{\code{29} Occupations in art, culture and sport}
#'       \item{\code{30} Support occupations in art, culture and sport}
#'       \item{\code{31} Retail sales and service supervisors and specialized occupations in sales and services}
#'       \item{\code{32} Occupations in sales and services}
#'       \item{\code{33} Sales and service representatives and other customer and personal services occupations}
#'       \item{\code{34} Sales and service support occupations}
#'       \item{\code{35} Technical trades and transportation officers and controllers}
#'       \item{\code{36} General trades}
#'       \item{\code{37} Mail and message distribution, other transport equipment operators and related maintenance workers}
#'       \item{\code{38} Helpers and labourers and other transport drivers, operators and labourers}
#'       \item{\code{39} Supervisors and occupations in natural resources, agriculture and related production}
#'       \item{\code{40} Workers and labourers in natural resources, agriculture and related production}
#'       \item{\code{41} Supervisors, central control and process operators in processing, manufacturing and utilities and aircraft assemblers and inspectors}
#'       \item{\code{42} Machine operators, assemblers and inspectors in processing, manufacturing and printing}
#'       \item{\code{43} Labourers in processing, manufacturing and utilities}
#'     }}
#'   \item{\code{YABSENT} \code{<fct>} Reason of absence, full week \code{for} Currently employed, absent from work}{
#'     \itemize{
#'       \item{\code{0} Other reasons}
#'       \item{\code{1} Own illness or disability}
#'       \item{\code{2} Personal or family responsibilities}
#'       \item{\code{3} Vacation}
#'     }}
#'   \item{\code{WKSAWAY} \code{<num>} Number of weeks absent from work [1, 99] \code{for} Currently employed, absent from work}
#'   \item{\code{PAYAWAY} \code{<fct>} Paid for time off, full-week absence only \code{for} Employees and self-employed - incorporated only}{
#'     \itemize{
#'       \item{\code{1} Yes}
#'       \item{\code{2} No}
#'     }}
#'   \item{\code{UHRSMAIN} \code{<num>} Usual hours worked per week at main job [0.1, 99] \code{for} Currently employed only}
#'   \item{\code{AHRSMAIN} \code{<num>} Actual hours worked per week at main job [0, 99] \code{for} Currently employed only}
#'   \item{\code{FTPTMAIN} \code{<fct>} Full- or part-time status at main or only job \code{for} Currently employed only}{
#'     \itemize{
#'       \item{\code{1} Full-time}
#'       \item{\code{2} Part-time}
#'     }}
#'   \item{\code{UTOTHRS} \code{<num>} Usual hours worked per week at all jobs [0.1, 99] \code{for} Currently employed only}
#'   \item{\code{ATOTHRS} \code{<num>} Actual hours worked per week at all jobs [0, 99] \code{for} Currently employed only}
#'   \item{\code{HRSAWAY} \code{<num>} Hours away from work, part-week absence only [0, 99] \code{for} Employees at work only}
#'   \item{\code{YAWAY} \code{<fct>} Reason for part-week absence \code{for} Employees at work only}{
#'     \itemize{
#'       \item{\code{0} Other reasons}
#'       \item{\code{1} Own illness or disability}
#'       \item{\code{2} Personal or family responsibilities}
#'       \item{\code{3} Vacation or civic holiday}
#'       \item{\code{4} Working short-time}
#'     }}
#'   \item{\code{PAIDOT} \code{<num>} Paid overtime hours in reference week [0, 990] \code{for} Employees at work only}
#'   \item{\code{UNPAIDOT} \code{<num>} Unpaid overtime hours in reference week [0, 990] \code{for} Employees at work only}
#'   \item{\code{XTRAHRS} \code{<num>} Number of overtime or extra hours worked [0, 99] \code{for} Employees at work only}
#'   \item{\code{WHYPT} \code{<fct>} Reason for part-time work \code{for} Currently employed, part-time usual work hours at their main or only job was below 30 per week}{
#'     \itemize{
#'       \item{\code{0} Other reasons}
#'       \item{\code{1} Own illness or disability}
#'       \item{\code{2} Caring for children}
#'       \item{\code{3} Other personal or family responsibilities}
#'       \item{\code{4} Going to school}
#'       \item{\code{5} Personal preference}
#'       \item{\code{6} Business conditions or could not find full-time work, looked for full-time work in last month}
#'       \item{\code{7} Business conditions or could not find full-time work, did not look for full-time work in last month}
#'     }}
#'   \item{\code{TENURE} \code{<num>} Job tenure with current employer [1, 240] \code{for} Currently employed only}
#'   \item{\code{PREVTEN} \code{<num>} Job tenure with previous employer [1, 240] \code{for} Not currently employed but worked in the past 12 months}
#'   \item{\code{HRLYEARN} \code{<num>} Usual hourly wages [3.69, 176.28] \code{for} Currently employed, employees}
#'   \item{\code{UNION} \code{<fct>} Union status \code{for} Currently employed, employees}{
#'     \itemize{
#'       \item{\code{1} Union member}
#'       \item{\code{2} Not a member but covered by a union contract or collective agreement}
#'       \item{\code{3} Non-unionized}
#'     }}
#'   \item{\code{PERMTEMP} \code{<fct>} Job permanency \code{for} Currently employed, employees}{
#'     \itemize{
#'       \item{\code{1} Permanent}
#'       \item{\code{2} Temporary, seasonal job}
#'       \item{\code{3} Temporary, term or contract job}
#'       \item{\code{4} Temporary, casual or other temporary jobs}
#'     }}
#'   \item{\code{ESTSIZE} \code{<fct>} Establishment size \code{for} Currently employed, employees}{
#'     \itemize{
#'       \item{\code{1} Less than 20 employees}
#'       \item{\code{2} 20 to 99 employees}
#'       \item{\code{3} 100 to 500 employees}
#'       \item{\code{4} More than 500 employees}
#'     }}
#'   \item{\code{FIRMSIZE} \code{<fct>} Firm size \code{for} Currently employed, employees}{
#'     \itemize{
#'       \item{\code{1} Less than 20 employees}
#'       \item{\code{2} 20 to 99 employees}
#'       \item{\code{3} 100 to 500 employees}
#'       \item{\code{4} More than 500 employees}
#'     }}
#'   \item{\code{DURUNEMP} \code{<num>} Duration of unemployment [1, 99] \code{for} Unemployed temporary layoffs and job searchers only}
#'   \item{\code{FLOWUNEM} \code{<fct>} Flows into unemployment \code{for} Currently unemployed}{
#'     \itemize{
#'       \item{\code{1} Job losers, temporary layoff}
#'       \item{\code{2} Job losers, permanent layoff}
#'       \item{\code{3} Job leavers}
#'       \item{\code{4} Job leavers/losers (status unknown), worked more than 1 year ago}
#'       \item{\code{5} New entrants}
#'       \item{\code{6} Re-entrants, worked 1 year ago or less}
#'       \item{\code{7} Re-entrants, worked more than 1 year ago}
#'       \item{\code{8} Future starts}
#'     }}
#'   \item{\code{UNEMFTPT} \code{<fct>} Job seekers by type of work sought and temporary layoffs by work status of last job \code{for} Currently unemployed}{
#'     \itemize{
#'       \item{\code{1} Full-time}
#'       \item{\code{2} Part-time}
#'       \item{\code{3} Future starts}
#'     }}
#'   \item{\code{WHYLEFTO} \code{<fct>} Reason for leaving job during previous year \code{for} Not currently employed but worked within the previous twelve months}{
#'     \itemize{
#'       \item{\code{0} Job leavers, other reasons}
#'       \item{\code{1} Job leavers, own illness or disability}
#'       \item{\code{2} Job leavers, personal or family responsibilities}
#'       \item{\code{3} Job leavers, going to school}
#'       \item{\code{4} Job losers, laid off}
#'       \item{\code{5} Job leavers, retired}
#'     }}
#'   \item{\code{WHYLEFTN} \code{<fct>} Reason for leaving job during previous year \code{for} Not currently employed but worked within the previous twelve months}{
#'     \itemize{
#'       \item{\code{0} Job leavers, other reasons}
#'       \item{\code{1} Job leavers, own illness or disability}
#'       \item{\code{2} Job leavers, caring for children}
#'       \item{\code{3} Job leavers, pregnancy}
#'       \item{\code{4} Job leavers, personal or family responsibilities}
#'       \item{\code{5} Job leavers, going to school}
#'       \item{\code{6} Job leavers, dissatisfied}
#'       \item{\code{7} Job leavers, retired}
#'       \item{\code{8} Job leavers, business sold or closed down (self-employed)}
#'       \item{\code{9} Job losers, end of seasonal job (employee)}
#'       \item{\code{10} Job losers, end of temporary or casual (employee)}
#'       \item{\code{11} Job losers, company moved or out of business (employee)}
#'       \item{\code{12} Job losers, business conditions (employee)}
#'       \item{\code{13} Job losers, dismissal or other reasons}
#'     }}
#'   \item{\code{DURJLESS} \code{<num>} Duration of joblessness [1, 240] \code{for} Not currently employed but worked at some time in the past}
#'   \item{\code{AVAILABL} \code{<fct>} Availability during the reference week \code{for} Temporary layoffs, job searchers and future starts, and those who wanted work but did not look because believes no suitable work available}{
#'     \itemize{
#'       \item{\code{1} Not available}
#'       \item{\code{2} Yes, available}
#'     }}
#'   \item{\code{LKPUBAG} \code{<num>} Unemployed, used public employment agency [1, 1] \code{for} Currently unemployed, job searchers}
#'   \item{\code{LKEMPLOY} \code{<num>} Unemployed, checked with employers directly [1, 1] \code{for} Currently unemployed, job searchers}
#'   \item{\code{LKRELS} \code{<num>} Unemployed, checked with friends or relatives [1, 1] \code{for} Currently unemployed, job searchers}
#'   \item{\code{LKATADS} \code{<num>} Unemployed, looked at job ads [1, 1] \code{for} Currently unemployed, job searchers}
#'   \item{\code{LKANSADS} \code{<num>} Unemployed, placed or answered ads [1, 1] \code{for} Currently unemployed, job searchers}
#'   \item{\code{LKOTHERN} \code{<num>} Unemployed, other methods [1, 1] \code{for} Currently unemployed, job searchers}
#'   \item{\code{PRIORACT} \code{<fct>} Main activity before started looking for work \code{for} Currently unemployed, job searchers}{
#'     \itemize{
#'       \item{\code{0} Other}
#'       \item{\code{1} Working}
#'       \item{\code{2} Managing a home}
#'       \item{\code{3} Going to school}
#'     }}
#'   \item{\code{YNOLOOK} \code{<fct>} Reason for not looking for work during the reference week \code{for} Persons not in the labour force who say they wanted a job in the reference week}{
#'     \itemize{
#'       \item{\code{0} Wanted work, reason - other}
#'       \item{\code{1} Wanted work, reason - own illness or disability}
#'       \item{\code{2} Wanted work, reason - caring for children}
#'       \item{\code{3} Wanted work, reason - other personal or family responsibilities}
#'       \item{\code{4} Wanted work, reason - school}
#'       \item{\code{5} Wanted work, reason - awaiting recall or reply}
#'       \item{\code{6} Wanted work, reason - discouraged}
#'     }}
#'   \item{\code{TLOLOOK} \code{<fct>} Temporary layoff, looked for work during the last four weeks \code{for} Currently unemployed, temporary layoffs}{
#'     \itemize{
#'       \item{\code{1} Yes}
#'       \item{\code{2} No}
#'     }}
#'   \item{\code{SCHOOLN} \code{<fct>} Current student status \code{for} Respondents aged 15 to 64 years}{
#'     \itemize{
#'       \item{\code{1} Non-student}
#'       \item{\code{2} Full-time student}
#'       \item{\code{3} Part-time student}
#'     }}
#'   \item{\code{EFAMTYPE} \code{<fct>} Type of economic family \code{for} All respondents}{
#'     \itemize{
#'       \item{\code{1} Person not in an economic family}
#'       \item{\code{2} Dual-earner couple, no children or none under 25}
#'       \item{\code{3} Dual-earner couple, youngest child 0 to 17}
#'       \item{\code{4} Dual-earner couple, youngest child 18 to 24}
#'       \item{\code{5} Single-earner couple, male employed, no children or none under 25}
#'       \item{\code{6} Single-earner couple, male employed, youngest child 0 to 17}
#'       \item{\code{7} Single-earner couple, male employed, youngest child 18 to 24}
#'       \item{\code{8} Single-earner couple, female employed, no children or none under 25}
#'       \item{\code{9} Single-earner couple, female employed, youngest child 0 to 17}
#'       \item{\code{10} Single-earner couple, female employed, youngest child 18 to 24}
#'       \item{\code{11} Non-earner couple, no children or none under 25}
#'       \item{\code{12} Non-earner couple, youngest child 0 to 17}
#'       \item{\code{13} Non-earner couple, youngest child 18 to 24}
#'       \item{\code{14} Lone-parent family, parent employed, youngest child 0 to 17}
#'       \item{\code{15} Lone-parent family, parent employed, youngest child 18 to 24}
#'       \item{\code{16} Lone-parent family, parent not employed, youngest child 0 to 17}
#'       \item{\code{17} Lone-parent family, parent not employed, youngest child 18 to 24}
#'       \item{\code{18} Other families}
#'     }}
#'   \item{\code{AGYOWNK} \code{<fct>} Age of youngest child \code{for} Parents only}{
#'     \itemize{
#'       \item{\code{1} Youngest child less than 6 years}
#'       \item{\code{2} Youngest child 6 to 12 years}
#'       \item{\code{3} Youngest child 13 to 17 years}
#'       \item{\code{4} Youngest child 18 to 24 years}
#'     }}
#'   \item{\code{FINALWT} \code{<num>} Standard final weight [1, 7207] \code{for} All respondents}
#' }
#' @export
read_lfs_pumf <- function(dir) {
    records <- .read_lfs_records(dir)
    codebook <- suppressWarnings(.read_lfs_codebook(dir))
    list(records = records, codebook = codebook)
}

#' @rdname read_lfs_pumf
.read_lfs_records <- function(dir) {
    csv_paths <- fs::dir_ls(dir, regexp = "pub.*csv")

    csv_paths |>
        purrr::map(data.table::fread) |>
        data.table::rbindlist() |>
        as.data.frame() |>
        tibble::as_tibble() |>
        .correct_decimal_placement()
}

#' @rdname read_lfs_pumf
#' @importFrom dplyr mutate across case_when filter group_by ungroup select
.correct_decimal_placement <- function(x) {
    x |>
        mutate(
            across(matches("HRS"), ~ .x / 10),
            across(matches("EARN"), ~ .x / 100)
        )
}

#' @rdname read_lfs_pumf
.read_lfs_codebook <- function(dir) {
    path <- glue::glue("{dir}/LFS_PUMF_EPA_FGMD_codebook.csv")

    d <- readr::read_csv(path, show_col_types = FALSE) |>
        janitor::clean_names() |>
        dplyr::transmute(
            var = variable_variable,
            field_id = field_champ,
            label = english_label_etiquette_anglais,
            universe = english_universe_univers_anglais
        ) |>
        dplyr::filter(
            !is.na(label),
            label != "Not applicable",
        ) |>
        dplyr::mutate(
            var_name = dplyr::case_when(
                is.na(field_id) ~ NA_character_,
                TRUE ~ toupper(var)
            ),
            var_label = dplyr::case_when(
                is.na(field_id) ~ NA_character_,
                TRUE ~ label
            ),
            var_universe = dplyr::case_when(
                is.na(field_id) ~ NA_character_,
                TRUE ~ universe
            ),
            factor_level = dplyr::case_when(
                !is.na(field_id) ~ NA_character_,
                TRUE ~ var
            ),
            factor_label = dplyr::case_when(
                !is.na(field_id) ~ NA_character_,
                TRUE ~ label
            )
        ) |>
        tidyr::fill(field_id, var_name, var_label, var_universe) |>
        dplyr::group_by(var_name) |>
        dplyr::mutate(
            is_factor = sum(!is.na(factor_level)) > 1,
            factor_level = dplyr::case_when(
                is_factor ~ as.integer(factor_level),
                TRUE ~ NA_integer_
            ),
            factor_label = dplyr::case_when(
                is_factor ~ factor_label,
                TRUE ~ NA_character_
            )
        ) |>
        dplyr::ungroup() |>
        dplyr::filter(
            !(is_factor & is.na(factor_level))
        ) |>
        dplyr::select(
            field_id,
            var_name,
            var_label,
            var_universe,
            is_factor,
            factor_level,
            factor_label
        )


    var_labels <- d |>
        dplyr::group_nest(field_id, var_name, var_label, var_universe, is_factor) |>
        dplyr::select(-data)

    factor_labels <- d |>
        dplyr::filter(is_factor) |>
        dplyr::select(-var_label, -is_factor, -var_universe) |>
        dplyr::group_nest(field_id, var_name, .key = "factor_labels") |>
        dplyr::mutate(factor_labels = purrr::map2(var_name, factor_labels, ~ {
            .y |>
                dplyr::mutate(
                    factor_label = forcats::fct_inorder(factor_label)
                ) |>
                setNames(c(glue::glue("{.x}"), glue::glue("{.x}_label")))
        }))

    suppressMessages(dplyr::left_join(var_labels, factor_labels))
}

#' Produce a data frame (tibble) with labelled vectors encoded as factors
#'
#' @param records A data frame (tibble) of records
#' @param codebook A data frame (tibble) of codebook
#' @return A data frame (tibble) with labelled vectors encoded as factors
#' @export
encode_lfs_factors <- function(records, codebook) {
    d <- records |>
        dplyr::mutate(
            NAICS_21_code = NAICS_21,
            NOC_10_code = NOC_10,
            NOC_43_code = NOC_43
        )
    factors <- codebook[codebook$is_factor, ]
    factor_labels <- factors$factor_labels
    d <- suppressMessages(
        purrr::reduce(factor_labels, .init = d, .f = dplyr::left_join)
    )

    d |>
        dplyr::select(-c(factors[["var_name"]])) |>
        dplyr::rename_all(~ stringr::str_remove(.x, "_label$")) |>
        dplyr::select(c(colnames(records), dplyr::matches("_code$")))
}

#' Produce data documentation in standard Roxygen markdown
#' @rdname roxygenize_lfs_codebook
#' @param records A data frame (tibble) of records
#' @param codebook A data frame (tibble) of codebook
#' @param year The year of the data
#' @param description A description of the data
#' @return A character vector of Roxygen markdown
#' @export
roxygenize_lfs_codebook <- function(records, codebook, year) {
    stringr::str_c(
        .cb_top(year),
        .cb_format(records, codebook),
        sep = "\n",
        collapse = "\n"
    )
}

#' Produce the top of data documentation
#' @rdname roxygenize_lfs_codebook
.cb_top <- function(year) {
    .cb_gen(
        title = glue::glue("Labour Force Survey Public Use Microdata File ({year})"),
        rdname = glue::glue("lfs_pumf_{year}"),
        description = .cb_describe(year),
        source = .cb_source(year)
    )
}

#' Generate roxygen documentation from key-value pairs
#' @rdname roxygenize_lfs_codebook
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> key-value pairs
.cb_gen <- function(...) {
    items <- rlang::list2(...)
    items |>
        purrr::imap_chr(~ {
            glue::glue("#' @{.y} {.x}")
        }) |>
        stringr::str_c(collapse = "\n")
}

#' Generate roxygen documentation for the format of a data set
#' @rdname roxygenize_lfs_codebook
.cb_format <- function(records, codebook) {
    head <- glue::glue(
        r"(The data set has <<nrow(records)>> rows and <<ncol(records)>> columns. \itemize{)",
        .open = "<<", .close = ">>"
    )

    data_dictionary <- codebook |>
        as.list() |>
        purrr::transpose() |>
        purrr::map(.cb_column, records) |>
        stringr::str_c(collapse = "\n")

    .cb_gen(
        format = glue::glue(
            "<<head>>",
            "<<data_dictionary>>",
            r"(#' })",
            .open = "<<", .close = ">>",
            .sep = "\n"
        )
    )
}


#' Generate roxygen documentation for a column of the data set
#' @rdname roxygenize_lfs_codebook
#' @param col a column from the codebook, as a named list with:
#' - `field_id`: the field ID
#' - `var_name`: the variable name
#' - `var_label`: the variable label
#' - `is_factor`: whether the variable is a factor
#' - `factor_labels`: if `is_factor`, a data frame (tibble) of factor labels
.cb_column <- function(col, records) {
    if (col$is_factor) {
        .cb_column__factor(col)
    } else {
        .cb_column__number(col, records)
    }
}

#' Generate roxygen documentation for a numeric column of the data set
#' @rdname roxygenize_lfs_codebook
.cb_column__number <- function(col, records) {
    col_range <- records[[col$var_name]] |> range(na.rm = TRUE)
    glue::glue(
        "#'   ",
        r"(\item{\code{<<col$var_name>>} \code{<num>} <<col$var_label>> [<<col_range[1]>>, <<col_range[2]>>] \code{for} <<col$var_universe>>})",
        .open = "<<", .close = ">>"
    )
}

#' Generate roxygen documentation for a factor column of the data set
#' @rdname roxygenize_lfs_codebook
.cb_column__factor <- function(col) {
    head <- glue::glue(
        "#'   ",
        r"(\item{\code{<<col$var_name>>} \code{<fct>} <<col$var_label>> \code{for} <<col$var_universe>>}{)",
        .open = "<<", .close = ">>"
    )

    levels <- col$factor_labels

    colnames(levels) <- c("factor_level", "factor_label")

    body <- levels |>
        dplyr::filter(!is.na(factor_level), !is.na(factor_label)) |>
        dplyr::mutate(factor_level = as.character(factor_level)) |>
        glue::glue_data(
            "#'       ",
            r"(\item{\code{<<factor_level>>} <<factor_label>>})",
            .open = "<<", .close = ">>"
        ) |>
        stringr::str_c(collapse = "\n")

    glue::glue(
        "<<head>>",
        r"(#'     \itemize{)",
        "<<body>>",
        r"(#'     }})",
        .open = "<<", .close = ">>",
        .sep = "\n"
    )
}

#' Truncate n-level factors to 7 levels
#' (levels[1, 2, 3], ..., levels[n-2, n-1, n])
#' @rdname roxygenize_lfs_codebook
.cb_truncate_factor <- function(factor_labels) {
    if (nrow(factor_labels) <= 7) {
        factor_labels
    } else {
        dplyr::bind_rows(
            dplyr::slice(factor_labels, 1:3),
            tibble::tibble(
                factor_level = "...",
                factor_label = "..."
            ),
            dplyr::slice(factor_labels, -c(3, 2, 1))
        )
    }
}


#' Generate roxygen documentation for the source of a data set
#' @rdname roxygenize_lfs_codebook
.cb_source <- function(year) {
    glue::glue(
        "Statistics Canada. ({{year}})",
        "Labour Force Survey Public Use Microdata File.",
        r"(\url{https://doi.org/10.25318/71m0001x-eng})",
        .open = "{{", .close = "}}"
    )
}

#' Generate roxygen documentation for the description of a data set
#' @rdname roxygenize_lfs_codebook
.cb_describe <- function(year) {
    glue::glue(
        "This public use microdata file (PUMF) contains",
        "non-aggregated data for a wide variety of variables",
        "collected from the Labour Force Survey (LFS) in {{year}}.",
        "The LFS collects monthly information on",
        "the labour market activities of Canada's working age population.",
        "This product is for users who prefer to do their own analysis",
        "by focusing on specific subgroups in the population",
        "or by cross-classifying variables that are not in",
        "our catalogued products.",
        "For more information about this survey",
        "(questionnaires, definitions, data sources and methods used):",
        r"(\href{https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=3701}{Labour Force Survey})",
        .open = "{{", .close = "}}", .sep = "\n#'   "
    )
}

#' @title Generate bootstrap weights for LFS PUMF data
#' @rdname generate_bootstrap_weights
#' @param d :data.frame (labelled) LFS PUMF data.
#' @param n_reps :integer number of bootstrap replicates to generate.
#' @return a (n_reps by nrow(d)) array of bootstrap weights.
#' @export
generate_lfs_bootstrap_weights <- function(d, n_reps) {
    uncalibrated_weights <- .generate_replicates(d$FINALWT, n_reps)
    age_tabs <- .calculate_age_tabs(d$AGE_6, d$AGE_12)
    domains <- interaction(d$SURVYEAR, d$SURVMNTH, d$PROV, d$SEX, age_tabs)

    .calibrate_weights(uncalibrated_weights, d$FINALWT, domains)
}


#' @title Sample poisson factors for simulated bootstrap weights
#' @rdname generate_bootstrap_weights
.sample_poisson_factors <- function(k) {
    sample(c(-1, 1), k, replace = TRUE)
}

#' @title initialize uncalibrated bootstrap replicate weights
#' @rdname generate_bootstrap_weights
.generate_replicates <- function(final_weight, n_reps) {
    k <- length(final_weight)
    adjustment_factors <- final_weight * sqrt((final_weight - 1) / final_weight)

    replicate(
        n_reps,
        final_weight + .sample_poisson_factors(k) * adjustment_factors,
        simplify = "array"
    )
}


#' @title Calibrate uncalibrated bootstrap replicate weights
#' @rdname generate_bootstrap_weights
.calibrate_weights <- function(uncalibrated_weights, final_weight, domains) {
    domain_indices <- split(seq_len(nrow(uncalibrated_weights)), domains)
    domain_fw_totals <- domain_indices |>
        sapply(function(x) sum(final_weight[x]))
    domain_bs_totals <- domain_indices |>
        sapply(function(x) colSums(uncalibrated_weights[x, ]))
    domain_scaling_factors <- domain_fw_totals / t(domain_bs_totals)

    unname(uncalibrated_weights * domain_scaling_factors[domains, ])
}

#' @title Calculate AGE_TABS for LFS PUMF data
#' @rdname generate_bootstrap_weights
#' @param age_6 :factor AGE_6 variable.
#' @param age_12 :factor AGE_12 variable.
#' @return a factor AGE_TABS variable.
#' @export
.calculate_age_tabs <- function(age_6, age_12) {
    age_tabs_base <- dplyr::case_when(
        age_6 %in% c("15 to 16 years", "17 to 19 years") ~ age_6,
        TRUE ~ age_12
    )
    forcats::fct_collapse(
        age_tabs_base,
        "35 to 44 years" = c("35 to 39 years", "40 to 44 years"),
        "45 to 54 years" = c("45 to 49 years", "50 to 54 years")
    )
}

#' @title quality measures for bootstrap estimates
#' @rdname bs_stat
.bs_out <- function(est_fw, est_bs) {
    bs_var <- mean((est_bs - est_fw)^2)
    bs_sd <- sqrt(bs_var)

    data.frame(
        est = est_fw,
        var = bs_var,
        se = bs_sd
    )
}

#' @title survey mean with bootstrap sampling variance and standard error
#' @description
#'     designed for use in a grouped dplyr::summarize() context
#'     where bootstrap weights are stored as an array column
#' @rdname bs_stat
#' @param x :number vector of survey data.
#' @param bootstrap_weights :array of bootstrap replicate weights.
#' @param final_weights :vector of final weights.
#' @return a data.frame with columns est, var, and se.
#' @export
lfs_bs_mean <- function(x, bootstrap_weights, final_weights) {
    est_fw <- sum(final_weights * x) / sum(final_weights)
    est_bs <- colSums(bootstrap_weights * x) / colSums(bootstrap_weights)
    .bs_out(est_fw, est_bs)
}

#' @title survey total with bootstrap sampling variance and standard error
#' @description
#'     designed for use in a grouped dplyr::summarize() context
#'     where bootstrap weights are stored as an array column
#' @rdname bs_stat
#' @param bootstrap_weights :array of bootstrap replicate weights.
#' @param final_weights :vector of final weights.
#' @return a data.frame with columns est, var, and se.
#' @export
lfs_bs_total <- function(bootstrap_weights, final_weights) {
    est_fw <- sum(final_weights)
    est_bs <- colSums(bootstrap_weights)
    .bs_out(est_fw, est_bs)
}
