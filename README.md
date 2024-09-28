# UKet

## 바로가기
> [API 명세서](https://dev.api.uket.site/swagger-ui/index.html#/)
> [관리자 API 명세서](https://uketadmin.p-e.kr/swagger-ui/index.html#/)

## 개발환경
| 구분 | 종류                             |
| --- |--------------------------------|
| Language | Java temurin 21.0.4            |
| Framework | Spring boot 3.2.4              |
| ORM | Spring Data JPA with Querydsl  |
| Build Tool | Gradle 8.4            |
| Database | MySQL 8.2.0, redis |
| API Docs | SpringDoc Swagger 3            |
| Test | JUnit 5                        |

## ERD
![uket-erd](https://github.com/DCNJ-Uket/Uket-BE/assets/127181370/cfdb6101-bee6-4200-b59c-ab3e56a012ca)

## 환경변수
**서버 측에 문의주세요**  

| 변수                   | 설명                        |
|----------------------|---------------------------|
| GOOGLE_CLIENT_ID     | 구글 로그인을 위한 client_id      |
| GOOGLE_CLIENT_SECRET | 구글 로그인을 위한 client_secret  |
| KAKAO_CLIENT_ID      | 카카오 로그인을 위한 client_id     |
| KAKAO_CLIENT_SECRET  | 카카오 로그인을 위한 client_secret |
| JWT_SECRET_KEY       | JWT 토큰 시그티처 키 값           |
| DEV_MYSQL_USERNAME   | 개발 서버 MySQL 사용자 명         |
| DEV_MYSQL_PASSWORD   | 개발 서버 MySQL 비밀번호          |
| SLACK_WEBHOOK_URL    | 에러 로깅을 위한 슬랙 Webhook url  |
| AWS_S3_ACCESS_KEY    | aws s3 IAM access-key     |
| AWS_S3_SECRET_KEY    | aws s3 IAM secret-key     |
| AWS_S3_BUCKET_KEY    | aws s3 bucket 이름          |
