/*Q1-----------*/
/*1*/ SELECT id, extra FROM SLEEP;
/*2*/ SELECT extra, id FROM SLEEP;
/*3*/ SELECT DISTINCT category FROM SLEEP;
/*4*/ SELECT id FROM SLEEP WHERE extra > 0;
/*5*/ SELECT category, SUM(extra) AS extraSum FROM SLEEP GROUP BY category;
/*6*/ SELECT category, COUNT(*) AS categoryNum FROM SLEEP GROUP BY category;
/*7*/ SELECT category, AVG(extra) AS mean_extra FROM SLEEP GROUP BY category;

/*Q2)----------*/
/*1*/ SELECT * FROM Department LIMIT 2;
/*2*/ SELECT employeename, hiredate, basewage FROM Employee;
/*3*/ SELECT basewage * wagelevel from Employee;
/*4*/ SELECT * FROM  Employee WHERE basewage BETWEEN 2000 AND 3000 ORDER BY basewage DESC;
/*5*/ SELECT employeename, hiredate, basewage FROM Employee WHERE hiredate > 6/10/2010 AND employeename LIKE '%8';
/*6*/ SELECT employeename, departmentid FROM Employee WHERE basewage * wagelevel > 7000;
/*7*/ SELECT departmentid, basewage FROM Employee WHERE basewage >= 3000 GROUP BY departmentid HAVING COUNT(*) > 2;
/*8*/ SELECT departmentid, AVG(basewage * wagelevel) FROM Employee GROUP BY departmentid ORDER BY AVG(basewage * wagelevel) ASC;
/*9*/ SELECT departmentid, employeesex, AVG(basewage * wagelevel) FROM Employee GROUP BY employeesex, departmentid ORDER BY departmentid DESC;
/*10*/ SELECT employeename, departmentname, principal FROM Employee JOIN Department ON Employee.departmentid = Department.departmentid;

/*Q3------------*/
/*1*/ curl http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data >> sqlHWDB.csv
      CREATE TABLE adult (Age float, Workclass char(255),Fnlwgt float, Education char(255), EducatioNum float, MaritalStatus char(255), Occupation char(255), Relationship char(255), Race char(255), Sex char(255), CapitalGain float, CapitalLoss float, HoursPerWeek float, NativeCountry char(255), Class char(255));
/*2*/ LOAD DATA LOCAL INFILE 'sqlHWDB.csv' INTO TABLE adult FIELDS TERMINATED BY ', ' OPTIONALLY ENCLOSED BY '"' LINES TERMINATED BY '\n' IGNORE 1 lines;
/*3*/ SELECT COUNT(*) FROM adult WHERE Age is null OR Workclass LIKE '?' OR Fnlwgt is null OR Education LIKE '?' OR EducatioNum is null OR MaritalStatus LIKE '?' OR Occupation LIKE '?' OR Relationship LIKE '?' OR Race LIKE '?' OR Sex LIKE '?' OR CapitalGain is null OR CapitalLoss is null OR HoursPerWeek is null OR NativeCountry LIKE '?' OR Class LIKE '?';
/*4*/ DELETE FROM adult WHERE Age is null OR Workclass LIKE '?' OR Fnlwgt is null OR Education LIKE '?' OR EducatioNum is null OR MaritalStatus LIKE '?' OR Occupation LIKE '?' OR Relationship LIKE '?' OR Race LIKE '?' OR Sex LIKE '?' OR CapitalGain is null OR CapitalLoss is null OR HoursPerWeek is null OR NativeCountry LIKE '?' OR Class LIKE '?';
/*5*/ SELECT ct_1/ct_2 FROM
      (SELECT class, COUNT(*) AS ct_1 FROM adult WHERE class = '<=50K') AS tbl_1,
      (SELECT class, COUNT(*) AS ct_2 FROM adult WHERE class = '>50K') AS tbl_2
      WHERE tbl_1.class <> tbl_2.class;
/*6*/ SELECT class,AVG(age) FROM adult GROUP BY class;
/*7*/ SELECT COUNT(*) FROM adult WHERE class = '>50K' AND age < 36.78;
/*8*/ SELECT class, AVG(HoursPerWeek) FROM adult GROUP BY class;
/*9*/ SELECT tbl_1.sex, ct_1/ct_2 FROM
      (SELECT sex, COUNT(*) AS ct_1 FROM adult WHERE class = '<=50K' GROUP BY sex) AS tbl_1,
      (SELECT sex, COUNT(*) AS ct_2 FROM adult WHERE class = '>50K' GROUP BY sex) AS tbl_2
      WHERE tbl_1.sex = tbl_2.sex;
