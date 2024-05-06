insert into university (name,email_post_fix) values ('일반인',null),('건국대학교','@konkuk.ac.kr'),('세종대학교','@sejong.ac.kr');
insert into events (name,start_date,end_date) values ('녹색지대',CURDATE(),CURDATE());
insert into shows (name,start_date,end_date,ticketing_date,total_ticket_count,location)
values ('DAY 1',CURDATE(),CURDATE(),CURDATE(),0,'자양동'),('DAY 2',CURDATE()+1,CURDATE()+1,CURDATE()+1,0,'자양동'),('DAY 3',CURDATE()+2,CURDATE()+2,CURDATE()+2,0,'자양동');
UPDATE university
    JOIN events ON university.name = '건국대학교' AND events.name = '녹색지대'
    SET university.current_event = events.event_id
WHERE university.name = '건국대학교';
UPDATE events
    JOIN university ON university.current_event = events.event_id
    SET events.university_id = university.university_id
WHERE university.current_event = events.event_id;
UPDATE shows
    JOIN events
SET shows.event_id = events.event_id
WHERE events.name = '녹색지대';
