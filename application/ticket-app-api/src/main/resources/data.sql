insert into university (name,email_post_fix) values ('일반인',null),('건국대학교','@konkuk.ac.kr'),('세종대학교','@sejong.ac.kr');
insert into events (name,start_date,end_date) values ('녹색지대',CURDATE(),CURDATE());
UPDATE university
    JOIN events ON university.name = '건국대학교' AND events.name = '녹색지대'
    SET university.current_event = events.event_id
WHERE university.name = '건국대학교';
UPDATE events
    JOIN university ON university.current_event = events.event_id
    SET events.university_id = university.university_id
WHERE university.current_event = events.event_id;
