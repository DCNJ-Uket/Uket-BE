insert into university (name, email_post_fix, logo_path)
values ('일반인', null, null),
       ('건국대학교', '@konkuk.ac.kr', 'konkuk.png'),
       ('세종대학교', '@sejong.ac.kr', null);

insert into events (name, start_date, end_date, location)
values ('녹색지대', CURDATE(), CURDATE(), '서울 광진구 능동로 120 건국대학교 노천극장'),
       ('대동제', CURDATE(), CURDATE(), '서울 광진구 능동로 120 세종대학교 노천극장');

insert into banner (event_id, path, title)
values ((select event_id from events e where e.name = '녹색지대'), 'konkuk/title.png', '축제 소개'),
       ((select event_id from events e where e.name = '녹색지대'), 'konkuk/artist.png', '라인업 소개');

INSERT INTO shows (event_id, name, start_date, end_date, ticketing_date, total_ticket_count, location)
SELECT e.event_id,
       CONCAT('DAY ', days.day),
       CURDATE() + days.offset,
       CURDATE() + days.offset,
       CURDATE() + days.offset,
       0,
       e.location
FROM events e
         CROSS JOIN (SELECT 0 AS offset, 1 AS day
                     UNION ALL
                     SELECT 1, 2
                     UNION ALL
                     SELECT 2, 3) days
WHERE e.name IN ('녹색지대', '대동제');

INSERT INTO reservation (show_id, start_time, end_time, reserved_count, total_count, type)
SELECT s.show_id,
       DATE_ADD(s.ticketing_date, INTERVAL CONCAT(t.hour, ':', t.minute) HOUR_MINUTE) AS start_time,
       DATE_ADD(s.ticketing_date, INTERVAL CONCAT(t.hour + ((t.minute + 20) DIV 60), ':', ((t.minute + 20) MOD 60))
                HOUR_MINUTE)                                                          AS end_time,
       0                                                                              AS reserved_count,
       100                                                                            AS total_count,
       CASE WHEN t.hour < 18 THEN 'TICKETING_STUDENT' ELSE 'TICKETING_ALL' END        AS type
FROM shows s
         JOIN events e ON s.event_id = e.event_id
         JOIN (SELECT 17 AS hour, minute
               FROM (SELECT 0 AS minute UNION ALL SELECT 20 UNION ALL SELECT 40) minutes
               UNION ALL
               SELECT 18 AS hour, minute
               FROM (SELECT 0 AS minute UNION ALL SELECT 20) minutes) t
WHERE e.name IN ('녹색지대', '대동제')
  AND s.name IN ('DAY 1', 'DAY 2', 'DAY 3');

UPDATE university
    JOIN events ON university.name = '건국대학교' AND events.name = '녹색지대'
SET university.current_event = events.event_id
WHERE university.name = '건국대학교';

UPDATE university
    JOIN events ON university.name = '세종대학교' AND events.name = '대동제'
SET university.current_event = events.event_id
WHERE university.name = '세종대학교';

UPDATE events
    JOIN university ON university.current_event = events.event_id
SET events.university_id = university.university_id
WHERE university.current_event = events.event_id;
