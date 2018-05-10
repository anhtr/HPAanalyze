hpa_export <- function(data, file_name, file_type = 'xlsx') {
    if(file_type == 'xlsx') {
        wb <- loadWorkbook(filename = file_name, create = TRUE)
        createSheet(wb, name = names(data))
        sheet_index = 0
        for (x in data) {
            sheet_index = sheet_index + 1
            writeWorksheet(wb, data = x, sheet = names(data)[sheet_index])
        }
        saveWorkbook(wb)
    }
}