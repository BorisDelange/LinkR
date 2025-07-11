% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_data_management.R
\name{add_patients_to_subset}
\alias{add_patients_to_subset}
\title{Add Patients to a Subset}
\usage{
add_patients_to_subset(patients = tibble::tibble(), subset_id = integer())
}
\arguments{
\item{patients}{Either:
- A numeric vector of `person_id`s  
- A data.frame or tibble with at least one of the following columns:
  - `person_id` (required): Patient identifier  
  - `visit_occurrence_id` (optional): Visit identifier, must be used with `person_id`  
  - `visit_detail_id` (optional): Visit detail identifier, must be used with `visit_occurrence_id`}

\item{subset_id}{An integer representing the ID of the subset to which patients will be added.
If not provided and the current page is `"subsets"`, it is inferred from currently selected subset.  
The value must be a positive integer.}
}
\value{
A character message indicating the result:
- Success message including the number of patients added
- Message indicating no new patients were added
- Error message if inputs are invalid
}
\description{
Adds one or more patients to a specific subset in the `subset_persons` table of the database.  
This function accepts either a vector of patient identifiers or a structured tibble including visit information.  
It ensures input validity and avoids adding duplicates already present in the subset.
}
\details{
The function performs several validation steps:

- Ensures `subset_id` is a valid positive integer  
- Converts a numeric vector of patients into a tibble with a `person_id` column  
- Validates the consistency of visit-related identifiers  
- Removes duplicates within the input and already existing entries in the database  

Supported use cases:
1. **Basic:** Only `person_id` is provided  
2. **With visit:** `person_id` and `visit_occurrence_id`  
3. **With visit details:** `person_id`, `visit_occurrence_id`, and `visit_detail_id`

Only new, unique entries will be inserted into the `subset_persons` table.
}
\examples{
\dontrun{
# Add patients using a simple vector of person IDs
patients <- c(123, 456, 789)
add_patients_to_subset(patients = patients)

# Add patients using a tibble with person IDs
patients <- tibble::tibble(person_id = c(123, 456, 789))
add_patients_to_subset(patients = patients)

# Add patients with visit_occurrence_id
patients <- tibble::tibble(
  person_id = c(123, 456),
  visit_occurrence_id = c(1001, 1002)
)
add_patients_to_subset(patients = patients)

# Add patients with visit_detail_id
patients <- tibble::tibble(
  person_id = c(123, 456),
  visit_occurrence_id = c(1001, 1002),
  visit_detail_id = c(2001, 2002)
)
add_patients_to_subset(patients = patients)
}

}
