###########################################
###########################################
# Due to volumne of results returned by
# microbiology query from Penn Data
# Store (PDS), this script processes
# 1,000,000 rows of data at a time
# to improve query performance.
###########################################
###########################################

import csv
import cx_Oracle
import datetime

base_dir = "C:/Users/Data/micro_cdiff"

#######################################################
# SQL query to pull Clostridioides difficile cultures
#######################################################

sql = "SELECT * FROM (SELECT rownum rn, M.* FROM ODS.MICROBIOLOGY M WHERE M.MICRO_CULTURE LIKE '%C. diff%')"

#######################################################
# enter credentials to connect to PDS
#######################################################

pds_conn = cx_Oracle.connect('USERNAME/PASSWORD')
pds_cursor = pds_conn.cursor()

#######################################################
# process data export in chunks to improve efficiency 
#######################################################

step = 1000000
for i in range(3):
    print("Processing chunk " + str(i) + "...")
    print(datetime.datetime.now())
    lower = i * step
    upper = (i + 1) * step
    sql2 = "WHERE rn >= " + str(lower) + " AND rn < " + str(upper)
    outfile = "python_sql_export_" + str(i) + ".csv"
    results = pds_cursor.execute(sql + sql2)
    with open(base_dir + outfile, 'wb') as f:
        writer = csv.writer(f)
        writer.writerows(results)

#######################################################        
# close connection to PDS        
#######################################################

pds_conn.close()
print("Finished.")
