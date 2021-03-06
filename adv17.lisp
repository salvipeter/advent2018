(defvar *data*)

(defun min-x ()
  (reduce #'min *data* :key (lambda (c) (if (consp (car c)) (caar c) (car c)))))
(defun max-x ()
  (reduce #'max *data* :key (lambda (c) (if (consp (car c)) (cdar c) (car c)))))

(defun min-y ()
  (reduce #'min *data* :key (lambda (c) (if (consp (cdr c)) (cadr c) (cdr c)))))
(defun max-y ()
  (reduce #'max *data* :key (lambda (c) (if (consp (cdr c)) (cddr c) (cdr c)))))

(defun generate-map ()
  (let ((m (make-array (list (+ (max-x) 2) (+ (max-y) 2)) :initial-element nil)))
    (dolist (c *data*)
      (if (consp (car c))
          (loop with y = (cdr c)
                for x from (caar c) to (cdar c)
                do (setf (aref m x y) 'clay))
          (loop with x = (car c)
                for y from (cadr c) to (cddr c)
                do (setf (aref m x y) 'clay))))
    m))

(defun enclosure (m x y)
  (labels ((rec (x dx)
             (if (or (eq (aref m x y) 'clay)
                     (null (aref m x (1+ y))))
                 x
                 (rec (+ x dx) dx))))
    (cons (rec (1- x) -1) (rec (1+ x) 1))))

(defun fill-up (m x y)
  "Fills up the reservoirs in M with WATER, starting from (X Y)."
  (when (< (1+ y) (array-dimension m 1))
    (if (aref m x (1+ y))
        (destructuring-bind (xleft . xright)
            (enclosure m x y)
          (cond ((and (eq (aref m xleft y) 'clay)
                      (eq (aref m xright y) 'clay))
                 (loop for x1 from (1+ xleft) below xright
                       do (setf (aref m x1 y) 'water))
                 (fill-up m x (1- y)))
                ((eq (aref m xleft y) 'clay)
                 (fill-up m xright (1+ y)))
                ((eq (aref m xright y) 'clay)
                 (fill-up m xleft (1+ y)))
                (t (fill-up m xleft (1+ y))
                   (fill-up m xright (1+ y)))))
        (fill-up m x (1+ y)))))

(defun flow (m x y)
  "Writes the water FLOW in M, starting from (X Y)."
  (when (< (1+ y) (array-dimension m 1))
    (case (aref m x (1+ y))
      (flow (setf (aref m x y) 'flow))
      ((clay water)
       (destructuring-bind (xleft . xright)
            (enclosure m x y)
          (loop for x1 from (1+ xleft) below xright
                do (setf (aref m x1 y) 'flow))
          (cond ((eq (aref m xleft y) 'clay)
                 (flow m xright y))
                ((eq (aref m xright y) 'clay)
                 (flow m xleft y))
                (t (flow m xleft y)
                   (flow m xright y)))))
      (t
       (setf (aref m x y) 'flow)
       (flow m x (1+ y))))))

(defun adv17 ()
  (let ((m (generate-map)))
    (fill-up m 500 0)
    (flow m 500 0)
    (loop with min-x = (min-x) and max-x = (max-x)
          with min-y = (min-y) and max-y = (max-y)
          for y from min-y to max-y
          sum (loop for x from (1- min-x) to (1+ max-x)
                    count (member (aref m x y) '(water flow))))))

(defun adv17b ()
  (let ((m (generate-map)))
    (fill-up m 500 0)
    (loop with min-x = (min-x) and max-x = (max-x)
          with min-y = (min-y) and max-y = (max-y)
          for y from min-y to max-y
          sum (loop for x from (1- min-x) to (1+ max-x)
                    count (eq (aref m x y) 'water)))))

;;; For debugging
(defun display-map (m)
  (loop for y from (min-y) to (max-y)
        do (loop for x from (1- (min-x)) to (1+ (max-x))
                 do (case (aref m x y)
                      (clay (format t "#"))
                      (water (format t "~~"))
                      (flow (format t "|"))
                      (t (format t "."))))
        (terpri)))

;;; Data

(defparameter *data*
  '(((336 . 349) . 1160)
    ((479 . 483) . 1111)
    ((409 . 419) . 1369)
    (413 . (698 . 701))
    (450 . (953 . 962))
    ((427 . 432) . 528)
    ((532 . 545) . 678)
    ((380 . 386) . 663)
    (511 . (595 . 623))
    (460 . (1251 . 1263))
    ((380 . 386) . 670)
    ((453 . 473) . 702)
    (340 . (1025 . 1040))
    (343 . (1249 . 1266))
    (455 . (1017 . 1039))
    ((527 . 534) . 1369)
    (400 . (1219 . 1227))
    ((336 . 341) . 1565)
    (358 . (245 . 257))
    ((447 . 451) . 845)
    (398 . (1194 . 1208))
    (429 . (1467 . 1480))
    ((384 . 386) . 544)
    (445 . (1027 . 1052))
    ((521 . 524) . 232)
    ((339 . 341) . 1614)
    (437 . (711 . 731))
    ((343 . 356) . 1377)
    ((513 . 537) . 1347)
    ((344 . 348) . 399)
    ((373 . 435) . 1601)
    (480 . (647 . 660))
    (350 . (89 . 94))
    ((438 . 458) . 7)
    ((522 . 549) . 1376)
    (466 . (1406 . 1415))
    (396 . (967 . 979))
    ((519 . 530) . 1215)
    (368 . (1188 . 1195))
    (356 . (506 . 514))
    (479 . (993 . 1011))
    (432 . (1383 . 1395))
    (489 . (966 . 974))
    ((458 . 544) . 426)
    (418 . (711 . 731))
    (348 . (8 . 21))
    (382 . (370 . 393))
    (451 . (439 . 447))
    (435 . (1596 . 1601))
    (407 . (1059 . 1070))
    ((448 . 450) . 802)
    ((350 . 361) . 1364)
    ((508 . 536) . 1616)
    ((539 . 545) . 1532)
    (397 . (658 . 678))
    (387 . (66 . 89))
    (493 . (1286 . 1289))
    ((543 . 546) . 1309)
    (468 . (1235 . 1244))
    (336 . (664 . 684))
    (531 . (307 . 319))
    (447 . (829 . 845))
    (496 . (1527 . 1540))
    (488 . (241 . 254))
    (436 . (916 . 922))
    (359 . (836 . 840))
    (376 . (74 . 94))
    (453 . (716 . 718))
    ((452 . 467) . 1183)
    ((520 . 522) . 62)
    (479 . (1063 . 1071))
    (502 . (1336 . 1344))
    ((543 . 549) . 1008)
    (352 . (1546 . 1559))
    (402 . (374 . 393))
    (388 . (494 . 515))
    (366 . (495 . 500))
    ((474 . 481) . 590)
    ((461 . 465) . 383)
    ((529 . 542) . 903)
    (484 . (156 . 165))
    (505 . (1337 . 1344))
    (507 . (1255 . 1268))
    (455 . (123 . 138))
    ((504 . 506) . 147)
    (521 . (97 . 108))
    (483 . (331 . 343))
    (424 . (1363 . 1375))
    (399 . (375 . 393))
    (337 . (378 . 402))
    (516 . (813 . 827))
    (432 . (29 . 31))
    ((343 . 367) . 1266)
    ((463 . 472) . 837)
    (537 . (1403 . 1420))
    (532 . (968 . 970))
    ((336 . 342) . 684)
    (375 . (1481 . 1490))
    ((458 . 476) . 518)
    ((428 . 436) . 629)
    (502 . (286 . 296))
    (546 . (1088 . 1093))
    (458 . (506 . 518))
    (424 . (335 . 351))
    (440 . (773 . 784))
    (493 . (869 . 874))
    (463 . (967 . 974))
    ((538 . 543) . 806)
    ((334 . 360) . 954)
    ((485 . 488) . 254)
    (379 . (842 . 864))
    (537 . (969 . 970))
    ((405 . 410) . 1518)
    ((495 . 512) . 150)
    (458 . (1209 . 1218))
    ((379 . 381) . 864)
    ((430 . 434) . 1324)
    (505 . (887 . 893))
    ((523 . 534) . 602)
    ((438 . 464) . 494)
    (435 . (39 . 54))
    (531 . (1017 . 1043))
    (441 . (1291 . 1302))
    (476 . (1487 . 1499))
    (452 . (174 . 179))
    ((488 . 511) . 750)
    (466 . (1146 . 1158))
    (361 . (408 . 426))
    ((377 . 398) . 1126)
    (473 . (80 . 88))
    (401 . (775 . 792))
    ((381 . 402) . 1165)
    (410 . (781 . 784))
    (435 . (1215 . 1231))
    (367 . (625 . 643))
    (486 . (1036 . 1046))
    (485 . (1414 . 1439))
    (544 . (157 . 162))
    ((533 . 536) . 1076)
    ((456 . 459) . 1241)
    ((348 . 361) . 1255)
    ((418 . 434) . 24)
    ((337 . 354) . 402)
    (436 . (1293 . 1307))
    ((345 . 347) . 877)
    (377 . (1036 . 1046))
    (434 . (125 . 140))
    (354 . (378 . 402))
    ((523 . 550) . 1462)
    (438 . (481 . 494))
    (447 . (290 . 300))
    (388 . (943 . 955))
    ((485 . 489) . 1439)
    (467 . (1171 . 1183))
    ((333 . 342) . 476)
    ((465 . 491) . 279)
    (489 . (1350 . 1359))
    (414 . (586 . 599))
    (459 . (892 . 905))
    (443 . (37 . 49))
    (342 . (1026 . 1040))
    (527 . (1369 . 1372))
    (461 . (1149 . 1161))
    (538 . (327 . 337))
    (358 . (1114 . 1122))
    (455 . (1408 . 1419))
    (406 . (1412 . 1414))
    ((422 . 448) . 536)
    ((410 . 432) . 1395)
    ((491 . 493) . 782)
    ((424 . 429) . 1280)
    ((395 . 418) . 59)
    ((476 . 489) . 1359)
    (419 . (774 . 792))
    (536 . (532 . 556))
    (492 . (807 . 810))
    (453 . (287 . 297))
    (545 . (443 . 451))
    (406 . (1311 . 1333))
    (421 . (1060 . 1070))
    (544 . (1148 . 1162))
    (522 . (74 . 88))
    ((378 . 389) . 1186)
    (437 . (170 . 179))
    (504 . (120 . 129))
    (454 . (504 . 515))
    (378 . (966 . 979))
    (522 . (1101 . 1109))
    ((446 . 455) . 138)
    (344 . (617 . 633))
    (466 . (690 . 699))
    (491 . (1153 . 1162))
    ((540 . 544) . 77)
    (442 . (1318 . 1330))
    (442 . (979 . 981))
    (466 . (954 . 958))
    ((415 . 441) . 847)
    ((353 . 355) . 1520)
    (353 . (190 . 213))
    ((520 . 533) . 1582)
    ((400 . 426) . 578)
    (383 . (1384 . 1392))
    ((451 . 454) . 69)
    (550 . (329 . 340))
    (467 . (1506 . 1517))
    ((486 . 489) . 1536)
    (446 . (1189 . 1198))
    ((333 . 353) . 438)
    ((437 . 441) . 1166)
    (544 . (1022 . 1047))
    (441 . (835 . 847))
    (489 . (661 . 689))
    (453 . (309 . 322))
    (385 . (1362 . 1372))
    (479 . (728 . 738))
    (489 . (1534 . 1536))
    (506 . (1496 . 1502))
    (521 . (727 . 747))
    (451 . (829 . 845))
    ((438 . 459) . 905)
    ((479 . 501) . 1071)
    (468 . (1211 . 1223))
    (369 . (1448 . 1459))
    (448 . (312 . 325))
    ((346 . 353) . 213)
    ((332 . 351) . 1622)
    ((351 . 367) . 47)
    ((442 . 445) . 1052)
    ((474 . 479) . 379)
    (498 . (117 . 126))
    (497 . (197 . 211))
    ((429 . 435) . 1231)
    ((527 . 534) . 1372)
    (385 . (1270 . 1293))
    (333 . (131 . 151))
    ((386 . 412) . 737)
    (360 . (1080 . 1094))
    ((344 . 346) . 147)
    (454 . (51 . 69))
    (414 . (1064 . 1067))
    (494 . (1093 . 1112))
    (418 . (1465 . 1473))
    (501 . (1062 . 1071))
    (386 . (730 . 737))
    (375 . (631 . 636))
    ((402 . 413) . 264)
    ((513 . 539) . 1450)
    ((515 . 518) . 37)
    (458 . (774 . 784))
    ((455 . 472) . 1039)
    ((415 . 417) . 902)
    ((371 . 385) . 1516)
    ((518 . 530) . 128)
    ((482 . 499) . 818)
    ((433 . 440) . 363)
    ((344 . 348) . 397)
    (524 . (197 . 211))
    (381 . (1270 . 1293))
    (516 . (359 . 361))
    (464 . (1428 . 1441))
    (452 . (1192 . 1201))
    (493 . (1210 . 1216))
    (488 . (734 . 750))
    ((378 . 396) . 979)
    (447 . (662 . 664))
    (390 . (823 . 831))
    (464 . (690 . 699))
    ((466 . 471) . 1158)
    (421 . (1465 . 1473))
    (387 . (1452 . 1456))
    (407 . (973 . 983))
    ((360 . 385) . 438)
    (534 . (1369 . 1372))
    (394 . (945 . 958))
    ((384 . 394) . 1249)
    (449 . (341 . 343))
    ((484 . 488) . 359)
    (423 . (990 . 1000))
    (344 . (397 . 399))
    (389 . (1329 . 1336))
    (464 . (481 . 494))
    (464 . (64 . 68))
    (337 . (616 . 633))
    (342 . (339 . 342))
    (476 . (1349 . 1359))
    ((418 . 421) . 1473)
    (447 . (953 . 962))
    (385 . (435 . 438))
    ((484 . 488) . 354)
    (423 . (1319 . 1330))
    (348 . (299 . 309))
    (440 . (354 . 363))
    (502 . (1579 . 1591))
    (434 . (6 . 24))
    ((522 . 545) . 1079)
    ((524 . 546) . 1093)
    (478 . (647 . 660))
    ((516 . 530) . 852)
    (371 . (326 . 339))
    (341 . (1170 . 1172))
    ((466 . 481) . 1280)
    ((446 . 448) . 512)
    (385 . (1500 . 1516))
    ((509 . 533) . 1330)
    ((441 . 446) . 1198)
    ((498 . 508) . 557)
    (543 . (327 . 337))
    (382 . (749 . 766))
    (457 . (442 . 453))
    (515 . (29 . 37))
    (426 . (1530 . 1543))
    ((497 . 502) . 296)
    (442 . (1148 . 1151))
    (487 . (596 . 623))
    (544 . (1476 . 1497))
    ((495 . 500) . 742)
    ((511 . 521) . 880)
    ((341 . 350) . 94)
    (449 . (439 . 447))
    (516 . (838 . 852))
    ((385 . 389) . 976)
    (543 . (134 . 145))
    (500 . (1118 . 1130))
    (488 . (946 . 951))
    (492 . (610 . 618))
    (518 . (1085 . 1095))
    (426 . (1554 . 1565))
    (400 . (84 . 86))
    ((447 . 460) . 757)
    (450 . (1105 . 1116))
    (458 . (570 . 589))
    (461 . (366 . 383))
    (378 . (1241 . 1255))
    ((345 . 354) . 296)
    (342 . (1590 . 1604))
    ((357 . 372) . 69)
    (495 . (158 . 168))
    (398 . (1510 . 1522))
    ((464 . 485) . 68)
    ((485 . 509) . 190)
    ((434 . 437) . 427)
    (492 . (999 . 1015))
    (465 . (1541 . 1545))
    (372 . (1240 . 1255))
    ((341 . 366) . 500)
    (523 . (1456 . 1462))
    ((540 . 544) . 1497)
    ((511 . 513) . 101)
    (473 . (335 . 337))
    ((487 . 507) . 1268)
    (332 . (330 . 351))
    (425 . (502 . 519))
    (415 . (836 . 847))
    (446 . (122 . 138))
    (390 . (159 . 167))
    (443 . (1345 . 1353))
    ((473 . 496) . 88)
    ((488 . 492) . 810)
    ((340 . 368) . 1195)
    ((436 . 460) . 1263)
    (508 . (54 . 65))
    ((421 . 442) . 1151)
    (455 . (309 . 322))
    (383 . (136 . 144))
    ((473 . 489) . 1463)
    ((340 . 345) . 1454)
    (342 . (1358 . 1361))
    ((540 . 550) . 558)
    (348 . (1255 . 1260))
    (518 . (123 . 128))
    (359 . (149 . 176))
    ((461 . 472) . 867)
    (537 . (140 . 142))
    (419 . (1358 . 1369))
    (393 . (693 . 704))
    ((447 . 460) . 755)
    ((406 . 426) . 1565)
    ((473 . 476) . 337)
    (503 . (634 . 642))
    ((503 . 508) . 1430)
    (526 . (993 . 1008))
    (441 . (1189 . 1198))
    (513 . (1427 . 1450))
    ((424 . 434) . 614)
    (532 . (460 . 475))
    (390 . (1309 . 1313))
    (353 . (837 . 840))
    ((367 . 383) . 643)
    (429 . (726 . 728))
    (466 . (1106 . 1116))
    (419 . (444 . 470))
    (440 . (660 . 662))
    ((433 . 452) . 1201)
    (395 . (823 . 831))
    (513 . (94 . 101))
    (364 . (949 . 966))
    (462 . (94 . 109))
    (402 . (797 . 816))
    ((498 . 500) . 919)
    ((408 . 426) . 1543)
    ((452 . 468) . 1223)
    ((411 . 416) . 345)
    (409 . (158 . 167))
    (489 . (1414 . 1439))
    (389 . (1021 . 1023))
    (463 . (214 . 222))
    (533 . (481 . 487))
    (450 . (1081 . 1094))
    (540 . (1476 . 1497))
    (367 . (79 . 97))
    (532 . (222 . 235))
    (461 . (1128 . 1137))
    (344 . (145 . 147))
    (373 . (631 . 636))
    (416 . (805 . 820))
    (483 . (527 . 539))
    ((417 . 476) . 1499)
    ((348 . 354) . 608)
    (431 . (726 . 728))
    ((333 . 354) . 151)
    (374 . (658 . 678))
    ((426 . 432) . 79)
    ((459 . 461) . 1179)
    (476 . (215 . 222))
    ((416 . 437) . 945)
    (441 . (1161 . 1166))
    (469 . (373 . 386))
    ((433 . 456) . 1145)
    ((436 . 452) . 1307)
    (443 . (660 . 662))
    (395 . (21 . 26))
    ((355 . 367) . 97)
    (368 . (1058 . 1070))
    (451 . (51 . 69))
    ((502 . 504) . 1591)
    ((436 . 438) . 822)
    (337 . (339 . 342))
    ((339 . 353) . 922)
    (540 . (71 . 77))
    (442 . (753 . 764))
    (399 . (136 . 144))
    (466 . (1258 . 1280))
    ((376 . 394) . 958)
    ((359 . 365) . 1427)
    (341 . (89 . 94))
    (519 . (754 . 755))
    ((423 . 442) . 1330)
    (477 . (1178 . 1203))
    ((431 . 434) . 1368)
    (516 . (72 . 85))
    (448 . (791 . 802))
    ((397 . 399) . 998)
    ((502 . 505) . 1344)
    (381 . (1139 . 1165))
    (437 . (956 . 965))
    ((354 . 356) . 514)
    (423 . (232 . 235))
    (434 . (738 . 741))
    (456 . (1232 . 1241))
    (521 . (860 . 880))
    (485 . (372 . 386))
    ((399 . 423) . 636)
    (345 . (853 . 877))
    (391 . (770 . 780))
    ((437 . 456) . 965)
    (409 . (1357 . 1369))
    ((472 . 478) . 1513)
    ((449 . 452) . 179)
    ((524 . 532) . 1273)
    ((442 . 470) . 764)
    (486 . (909 . 931))
    (531 . (330 . 340))
    (361 . (1547 . 1559))
    ((396 . 424) . 351)
    ((441 . 463) . 723)
    (496 . (80 . 88))
    (420 . (1034 . 1043))
    (412 . (781 . 784))
    (409 . (126 . 140))
    (351 . (31 . 47))
    (537 . (1340 . 1347))
    ((392 . 411) . 912)
    ((530 . 532) . 475)
    ((413 . 470) . 558)
    (433 . (354 . 363))
    ((510 . 526) . 44)
    (379 . (370 . 393))
    (410 . (1193 . 1208))
    ((370 . 373) . 1534)
    (439 . (738 . 741))
    (413 . (244 . 264))
    (355 . (80 . 97))
    (472 . (1509 . 1513))
    (370 . (949 . 966))
    ((337 . 364) . 1072)
    (491 . (770 . 782))
    (380 . (877 . 890))
    ((398 . 403) . 1346)
    (487 . (1179 . 1203))
    ((537 . 539) . 1420)
    ((389 . 403) . 1021)
    (341 . (654 . 660))
    (542 . (397 . 410))
    ((423 . 426) . 1000)
    (538 . (502 . 508))
    ((467 . 486) . 1517)
    (353 . (908 . 922))
    (417 . (1509 . 1522))
    (471 . (629 . 641))
    (354 . (339 . 356))
    (380 . (663 . 670))
    (417 . (736 . 752))
    (412 . (1064 . 1067))
    (334 . (1509 . 1530))
    ((410 . 412) . 781)
    (412 . (1107 . 1113))
    (467 . (311 . 325))
    (456 . (955 . 965))
    (355 . (1520 . 1527))
    (480 . (790 . 791))
    (512 . (140 . 150))
    (341 . (977 . 991))
    (395 . (750 . 766))
    ((370 . 373) . 719)
    ((436 . 455) . 1397)
    ((353 . 356) . 633)
    (486 . (770 . 786))
    ((450 . 466) . 1116)
    (344 . (654 . 660))
    ((424 . 433) . 1028)
    (436 . (1383 . 1397))
    (339 . (1614 . 1617))
    ((345 . 351) . 179)
    (424 . (1230 . 1243))
    (492 . (707 . 717))
    ((434 . 437) . 889)
    (500 . (915 . 919))
    ((356 . 366) . 1018)
    ((503 . 508) . 642)
    (472 . (815 . 837))
    ((387 . 407) . 1002)
    ((507 . 532) . 714)
    (436 . (503 . 515))
    ((532 . 537) . 970)
    (533 . (1562 . 1582))
    (454 . (397 . 410))
    (540 . (517 . 540))
    (351 . (1611 . 1622))
    (498 . (225 . 228))
    (452 . (1212 . 1223))
    (332 . (1611 . 1622))
    (486 . (156 . 165))
    (488 . (225 . 228))
    (465 . (911 . 912))
    (502 . (1383 . 1394))
    (393 . (1329 . 1336))
    ((483 . 492) . 1015)
    (486 . (1534 . 1536))
    (469 . (1406 . 1415))
    ((514 . 516) . 85)
    (456 . (1343 . 1356))
    ((340 . 351) . 575)
    ((482 . 509) . 232)
    ((471 . 473) . 733)
    (387 . (576 . 582))
    (511 . (735 . 750))
    (373 . (714 . 719))
    (485 . (65 . 68))
    (432 . (377 . 393))
    ((398 . 417) . 1522)
    ((374 . 397) . 678)
    (385 . (943 . 955))
    (360 . (687 . 711))
    (520 . (262 . 275))
    (416 . (338 . 345))
    ((463 . 465) . 1545)
    ((484 . 491) . 1162)
    (341 . (495 . 500))
    ((470 . 483) . 959)
    ((513 . 516) . 359)
    (413 . (1468 . 1480))
    (348 . (329 . 351))
    ((435 . 461) . 1137)
    ((370 . 373) . 1378)
    ((376 . 382) . 94)
    ((441 . 457) . 453)
    ((380 . 406) . 1478)
    ((520 . 523) . 785)
    (470 . (943 . 959))
    ((445 . 449) . 343)
    (539 . (1404 . 1420))
    (514 . (72 . 85))
    ((436 . 454) . 515)
    (508 . (635 . 642))
    (488 . (354 . 359))
    (509 . (219 . 232))
    ((389 . 398) . 193)
    (515 . (993 . 1008))
    (469 . (26 . 42))
    ((506 . 522) . 88)
    ((498 . 500) . 915)
    ((518 . 536) . 556)
    (384 . (1247 . 1249))
    (394 . (1247 . 1249))
    ((504 . 521) . 108)
    (427 . (913 . 925))
    ((360 . 450) . 1094)
    (458 . (424 . 426))
    ((444 . 464) . 1441)
    ((442 . 464) . 981)
    (444 . (913 . 925))
    (399 . (420 . 426))
    (412 . (729 . 737))
    (413 . (545 . 558))
    ((455 . 458) . 1110)
    ((396 . 400) . 86)
    (487 . (628 . 641))
    ((387 . 404) . 1398)
    ((527 . 530) . 545)
    (479 . (370 . 379))
    (474 . (370 . 379))
    (534 . (589 . 602))
    (389 . (172 . 193))
    (514 . (1556 . 1572))
    (367 . (1034 . 1041))
    (496 . (262 . 275))
    (487 . (1551 . 1552))
    (361 . (1352 . 1364))
    (385 . (964 . 976))
    ((348 . 365) . 21)
    (487 . (1255 . 1268))
    (398 . (1119 . 1126))
    ((447 . 459) . 664)
    (353 . (615 . 633))
    (476 . (335 . 337))
    (380 . (1468 . 1478))
    (550 . (1456 . 1462))
    ((434 . 439) . 738)
    (545 . (1058 . 1079))
    ((491 . 508) . 300)
    (504 . (96 . 108))
    (493 . (769 . 782))
    ((348 . 373) . 309)
    (497 . (244 . 257))
    (346 . (190 . 213))
    (421 . (1148 . 1151))
    ((461 . 470) . 447)
    (379 . (807 . 818))
    (354 . (271 . 296))
    ((506 . 531) . 1484)
    (416 . (935 . 945))
    (506 . (1115 . 1125))
    (366 . (994 . 1018))
    (548 . (516 . 540))
    (436 . (820 . 822))
    (398 . (1345 . 1346))
    (334 . (933 . 954))
    ((519 . 536) . 1305)
    ((499 . 522) . 1109)
    (448 . (525 . 536))
    (351 . (159 . 179))
    (538 . (443 . 451))
    ((483 . 488) . 539)
    ((475 . 478) . 105)
    ((385 . 387) . 1456)
    ((463 . 489) . 974)
    (336 . (1155 . 1160))
    (539 . (1149 . 1162))
    ((493 . 500) . 1289)
    (345 . (270 . 296))
    (452 . (270 . 278))
    ((449 . 451) . 447)
    ((393 . 421) . 704)
    ((403 . 405) . 904)
    (416 . (1551 . 1559))
    (550 . (550 . 558))
    (530 . (1199 . 1215))
    (497 . (286 . 296))
    (342 . (663 . 684))
    (523 . (387 . 389))
    (481 . (1036 . 1046))
    (485 . (1570 . 1581))
    (429 . (461 . 472))
    ((430 . 444) . 827)
    (464 . (351 . 356))
    (381 . (841 . 864))
    (526 . (134 . 145))
    (533 . (1311 . 1330))
    (518 . (672 . 690))
    ((488 . 498) . 225)
    ((477 . 487) . 1203)
    (339 . (908 . 922))
    ((479 . 486) . 786)
    ((461 . 477) . 1161)
    ((340 . 342) . 1040)
    (354 . (130 . 151))
    ((438 . 456) . 807)
    (337 . (1357 . 1361))
    ((508 . 523) . 389)
    ((364 . 378) . 1313)
    (478 . (101 . 105))
    (459 . (1176 . 1179))
    (531 . (140 . 142))
    (434 . (417 . 427))
    (480 . (243 . 257))
    ((410 . 431) . 1352)
    ((462 . 484) . 109)
    ((353 . 355) . 1527)
    ((506 . 514) . 1125)
    (387 . (10 . 13))
    ((342 . 357) . 1246)
    (411 . (898 . 912))
    ((383 . 395) . 26)
    (501 . (1555 . 1572))
    (345 . (463 . 482))
    ((471 . 473) . 731)
    (425 . (171 . 179))
    (484 . (1152 . 1162))
    (371 . (1500 . 1516))
    ((332 . 348) . 351)
    ((367 . 386) . 1493)
    (476 . (505 . 518))
    ((383 . 399) . 144)
    (539 . (1513 . 1532))
    ((373 . 400) . 113)
    (471 . (352 . 362))
    ((400 . 402) . 1179)
    (408 . (1531 . 1543))
    (495 . (117 . 126))
    (384 . (251 . 277))
    ((404 . 406) . 1414)
    (465 . (365 . 383))
    (424 . (1270 . 1280))
    (465 . (289 . 300))
    (506 . (946 . 951))
    (392 . (575 . 582))
    ((454 . 542) . 410)
    (483 . (1105 . 1111))
    (448 . (501 . 512))
    ((462 . 479) . 738)
    ((480 . 502) . 474)
    ((427 . 444) . 925)
    ((512 . 532) . 235)
    ((459 . 480) . 791)
    (506 . (143 . 147))
    (435 . (1127 . 1137))
    (456 . (287 . 297))
    (463 . (1326 . 1337))
    ((490 . 494) . 1112)
    (508 . (388 . 389))
    ((377 . 415) . 1026)
    (367 . (1483 . 1493))
    ((440 . 443) . 662)
    (365 . (1383 . 1392))
    (450 . (791 . 802))
    (452 . (771 . 781))
    ((380 . 400) . 890)
    (382 . (297 . 320))
    ((480 . 497) . 257)
    (514 . (1115 . 1125))
    (460 . (755 . 757))
    (453 . (570 . 589))
    (402 . (1174 . 1179))
    (459 . (789 . 791))
    ((463 . 469) . 1337)
    (519 . (1298 . 1305))
    ((424 . 434) . 616)
    (423 . (736 . 752))
    ((384 . 392) . 277)
    (532 . (708 . 714))
    (364 . (1307 . 1313))
    (336 . (688 . 711))
    ((342 . 497) . 1604)
    ((415 . 418) . 1237)
    (546 . (1023 . 1047))
    (422 . (294 . 314))
    ((464 . 472) . 155)
    (417 . (1487 . 1499))
    (353 . (436 . 438))
    ((420 . 423) . 1043)
    ((357 . 361) . 38)
    (523 . (759 . 785))
    (459 . (1232 . 1241))
    ((402 . 422) . 314)
    (453 . (692 . 702))
    (462 . (330 . 343))
    (358 . (1171 . 1172))
    (378 . (1176 . 1186))
    (530 . (123 . 128))
    ((504 . 518) . 690)
    ((538 . 543) . 337)
    (535 . (156 . 162))
    (444 . (817 . 827))
    (508 . (909 . 931))
    (361 . (111 . 122))
    (396 . (336 . 351))
    (455 . (670 . 684))
    (477 . (1148 . 1161))
    (433 . (1491 . 1495))
    (503 . (978 . 999))
    (387 . (1379 . 1398))
    (387 . (990 . 1002))
    (432 . (63 . 79))
    ((416 . 426) . 820)
    ((440 . 443) . 49)
    (504 . (671 . 690))
    (513 . (1339 . 1347))
    (509 . (1312 . 1330))
    (471 . (1146 . 1158))
    (360 . (436 . 438))
    (333 . (436 . 438))
    (433 . (1017 . 1028))
    (384 . (793 . 796))
    (382 . (5 . 16))
    (471 . (731 . 733))
    ((506 . 509) . 472)
    (391 . (1489 . 1504))
    (368 . (661 . 671))
    (481 . (573 . 590))
    (402 . (1139 . 1165))
    ((404 . 425) . 519)
    ((389 . 403) . 1023)
    ((456 . 461) . 1356)
    ((487 . 504) . 129)
    (385 . (875 . 887))
    ((495 . 498) . 126)
    ((403 . 428) . 224)
    (479 . (1105 . 1111))
    ((501 . 514) . 1572)
    (417 . (879 . 902))
    (357 . (36 . 38))
    (437 . (1161 . 1166))
    (384 . (529 . 544))
    (357 . (65 . 69))
    (476 . (1528 . 1540))
    (436 . (627 . 629))
    (482 . (219 . 232))
    (415 . (1013 . 1026))
    (428 . (200 . 224))
    (456 . (1143 . 1145))
    (438 . (820 . 822))
    (459 . (233 . 255))
    (524 . (227 . 232))
    ((492 . 498) . 610)
    ((443 . 552) . 430)
    (370 . (1034 . 1041))
    (502 . (469 . 474))
    (464 . (152 . 155))
    (437 . (1346 . 1353))
    (536 . (1298 . 1305))
    (472 . (1016 . 1039))
    ((478 . 493) . 1216)
    ((391 . 393) . 1504)
    (533 . (264 . 266))
    (491 . (289 . 300))
    (405 . (901 . 904))
    (507 . (709 . 714))
    (480 . (1495 . 1502))
    ((511 . 538) . 1250)
    (397 . (4 . 16))
    (423 . (40 . 54))
    ((478 . 495) . 168)
    ((476 . 496) . 1540)
    (481 . (1258 . 1280))
    (385 . (1218 . 1227))
    ((453 . 456) . 297)
    (465 . (268 . 279))
    ((458 . 462) . 1218)
    ((382 . 385) . 320)
    (437 . (936 . 945))
    ((335 . 354) . 1473)
    (433 . (916 . 922))
    ((418 . 437) . 731)
    (370 . (1351 . 1378))
    ((337 . 342) . 1361)
    ((337 . 342) . 342)
    ((334 . 358) . 257)
    (455 . (1103 . 1110))
    ((341 . 358) . 1172)
    ((434 . 439) . 741)
    ((490 . 510) . 20)
    ((427 . 432) . 530)
    (508 . (1594 . 1616))
    (380 . (1429 . 1439))
    (447 . (29 . 31))
    (529 . (885 . 903))
    (520 . (1562 . 1582))
    (545 . (659 . 678))
    ((387 . 392) . 582)
    (461 . (848 . 867))
    ((459 . 464) . 476)
    (386 . (530 . 544))
    (473 . (693 . 702))
    (508 . (288 . 300))
    ((409 . 434) . 140)
    ((425 . 437) . 179)
    ((441 . 446) . 1302)
    (528 . (55 . 65))
    (356 . (1367 . 1377))
    ((521 . 533) . 487)
    (540 . (551 . 558))
    (442 . (1026 . 1052))
    (511 . (94 . 101))
    (356 . (1570 . 1581))
    ((363 . 372) . 241)
    (435 . (1037 . 1050))
    ((363 . 367) . 815)
    ((388 . 390) . 515)
    ((527 . 535) . 1195)
    (478 . (1509 . 1513))
    (438 . (793 . 807))
    (473 . (1444 . 1463))
    (441 . (714 . 723))
    (457 . (352 . 356))
    ((544 . 550) . 1448)
    (365 . (1408 . 1427))
    (365 . (464 . 482))
    ((450 . 468) . 1244)
    (439 . (376 . 393))
    ((420 . 431) . 841)
    (488 . (526 . 539))
    (506 . (75 . 88))
    (368 . (759 . 760))
    ((429 . 456) . 1460)
    (400 . (878 . 890))
    (348 . (1342 . 1347))
    ((356 . 379) . 818)
    ((487 . 492) . 1552)
    ((526 . 543) . 145)
    (346 . (145 . 147))
    ((424 . 445) . 1375)
    (386 . (1484 . 1493))
    ((436 . 457) . 866)
    (363 . (215 . 241))
    ((515 . 526) . 1008)
    (414 . (1551 . 1559))
    ((379 . 382) . 393)
    ((415 . 435) . 1050)
    (332 . (822 . 829))
    (369 . (153 . 169))
    (389 . (1176 . 1186))
    (475 . (1409 . 1419))
    (418 . (1273 . 1283))
    (539 . (291 . 312))
    (485 . (178 . 190))
    (546 . (1297 . 1309))
    (334 . (246 . 257))
    (440 . (37 . 49))
    (543 . (991 . 1008))
    (484 . (1117 . 1130))
    ((489 . 491) . 689)
    (420 . (232 . 235))
    (462 . (1209 . 1218))
    (438 . (893 . 905))
    (391 . (875 . 887))
    (399 . (610 . 620))
    (504 . (349 . 365))
    ((440 . 458) . 784)
    (448 . (771 . 781))
    (500 . (742 . 747))
    (436 . (1272 . 1283))
    (538 . (1210 . 1223))
    (521 . (480 . 487))
    ((418 . 436) . 1283)
    (409 . (1230 . 1243))
    ((471 . 487) . 641)
    ((369 . 374) . 169)
    ((359 . 384) . 176)
    ((413 . 429) . 1480)
    ((533 . 536) . 1073)
    (354 . (506 . 514))
    (391 . (1549 . 1554))
    ((490 . 493) . 874)
    ((469 . 479) . 42)
    (393 . (1448 . 1459))
    (365 . (7 . 21))
    (400 . (565 . 578))
    (504 . (143 . 147))
    (472 . (848 . 867))
    (410 . (444 . 470))
    (533 . (1073 . 1076))
    ((373 . 392) . 935)
    (341 . (1614 . 1617))
    (345 . (158 . 179))
    (333 . (456 . 476))
    (487 . (119 . 129))
    (403 . (199 . 224))
    (474 . (572 . 590))
    (433 . (1144 . 1145))
    (479 . (526 . 528))
    (431 . (1368 . 1371))
    (470 . (545 . 558))
    (498 . (610 . 618))
    ((447 . 450) . 962)
    ((414 . 416) . 1559)
    ((420 . 435) . 661)
    ((373 . 375) . 631)
    (508 . (544 . 557))
    (382 . (75 . 94))
    ((469 . 486) . 663)
    (491 . (662 . 689))
    (387 . (1209 . 1212))
    (378 . (1306 . 1313))
    (429 . (734 . 744))
    (399 . (996 . 998))
    (434 . (881 . 889))
    ((447 . 459) . 255)
    (393 . (1489 . 1504))
    (489 . (1444 . 1463))
    (347 . (853 . 877))
    (457 . (853 . 866))
    ((382 . 397) . 16)
    (470 . (873 . 883))
    (510 . (1016 . 1043))
    (464 . (980 . 981))
    (397 . (996 . 998))
    (483 . (944 . 959))
    (490 . (10 . 20))
    ((462 . 466) . 1389)
    ((531 . 537) . 142)
    (538 . (1234 . 1250))
    ((385 . 391) . 887)
    (417 . (1406 . 1418))
    (431 . (1342 . 1352))
    (381 . (326 . 339))
    (519 . (1199 . 1215))
    (345 . (1437 . 1454))
    (509 . (464 . 472))
    (391 . (1057 . 1070))
    ((404 . 406) . 1412)
    (509 . (178 . 190))
    (362 . (339 . 356))
    (374 . (153 . 169))
    ((383 . 391) . 230)
    ((356 . 485) . 1581)
    (421 . (692 . 704))
    ((492 . 498) . 618)
    (504 . (1578 . 1591))
    (452 . (1172 . 1183))
    (544 . (1435 . 1448))
    (422 . (525 . 536))
    (456 . (716 . 718))
    (336 . (1540 . 1565))
    ((407 . 421) . 1070)
    ((527 . 530) . 542)
    (436 . (852 . 866))
    ((387 . 389) . 1212)
    ((429 . 431) . 728)
    (499 . (798 . 818))
    (498 . (915 . 919))
    (530 . (460 . 475))
    ((508 . 528) . 65)
    ((502 . 510) . 1394)
    ((436 . 438) . 820)
    (495 . (141 . 150))
    (396 . (1308 . 1313))
    ((463 . 466) . 958)
    ((390 . 396) . 1313)
    (472 . (152 . 155))
    (438 . (586 . 599))
    (405 . (475 . 499))
    (510 . (1383 . 1394))
    (354 . (1469 . 1473))
    (359 . (1407 . 1427))
    ((448 . 467) . 325)
    (446 . (501 . 512))
    (506 . (463 . 472))
    (430 . (1324 . 1327))
    ((390 . 395) . 831)
    (403 . (1344 . 1346))
    (491 . (268 . 279))
    (506 . (1473 . 1484))
    (377 . (1119 . 1126))
    ((486 . 508) . 931)
    (360 . (933 . 954))
    (497 . (1590 . 1604))
    ((452 . 455) . 278)
    (361 . (36 . 38))
    (406 . (1469 . 1478))
    (526 . (31 . 44))
    ((433 . 454) . 1495)
    (363 . (976 . 991))
    (488 . (807 . 810))
    ((382 . 391) . 1554)
    ((372 . 378) . 1255)
    ((481 . 486) . 1046)
    ((455 . 475) . 1419)
    (490 . (870 . 874))
    (407 . (698 . 701))
    (404 . (502 . 519))
    (527 . (1173 . 1195))
    (523 . (588 . 602))
    (458 . (4 . 7))
    ((429 . 451) . 472)
    (499 . (1100 . 1109))
    ((508 . 518) . 1095)
    (480 . (670 . 684))
    (400 . (1174 . 1179))
    ((385 . 388) . 955)
    (549 . (1356 . 1376))
    (504 . (814 . 827))
    (475 . (101 . 105))
    (451 . (874 . 883))
    (356 . (661 . 671))
    (365 . (770 . 780))
    (410 . (1341 . 1352))
    (428 . (628 . 629))
    (481 . (910 . 912))
    (357 . (1241 . 1246))
    (484 . (354 . 359))
    (510 . (32 . 44))
    (427 . (528 . 530))
    ((346 . 368) . 760)
    ((521 . 525) . 747)
    ((385 . 400) . 1227)
    ((394 . 409) . 1190)
    (530 . (542 . 545))
    (486 . (1507 . 1517))
    ((430 . 434) . 1327)
    ((334 . 361) . 1530)
    (479 . (769 . 786))
    (376 . (946 . 958))
    (536 . (1073 . 1076))
    ((531 . 550) . 340)
    ((495 . 500) . 747)
    (541 . (94 . 117))
    (495 . (742 . 747))
    (486 . (650 . 663))
    (415 . (879 . 902))
    ((477 . 531) . 319)
    (540 . (1210 . 1223))
    (423 . (1034 . 1043))
    ((415 . 418) . 1240)
    (376 . (419 . 426))
    (373 . (1522 . 1534))
    (498 . (543 . 557))
    (466 . (1377 . 1389))
    ((365 . 383) . 1392)
    (462 . (1377 . 1389))
    ((463 . 476) . 222)
    (409 . (1170 . 1190))
    ((368 . 391) . 1070)
    (535 . (1173 . 1195))
    (348 . (519 . 537))
    ((488 . 506) . 951)
    ((455 . 480) . 684)
    ((487 . 511) . 623)
    ((488 . 498) . 228)
    ((409 . 424) . 1243)
    ((464 . 466) . 699)
    (532 . (753 . 755))
    (415 . (1036 . 1050))
    ((401 . 419) . 792)
    (377 . (1013 . 1026))
    ((337 . 358) . 1122)
    ((410 . 412) . 784)
    (366 . (794 . 796))
    ((387 . 389) . 13)
    ((544 . 546) . 1047)
    (386 . (663 . 670))
    (337 . (1114 . 1122))
    ((400 . 426) . 867)
    (544 . (72 . 77))
    (438 . (4 . 7))
    (512 . (979 . 999))
    (389 . (10 . 13))
    (527 . (542 . 545))
    (404 . (1412 . 1414))
    (455 . (1384 . 1397))
    (346 . (112 . 122))
    (361 . (1255 . 1260))
    ((466 . 469) . 1415)
    (497 . (351 . 362))
    (426 . (989 . 1000))
    (389 . (964 . 976))
    (411 . (1310 . 1333))
    (426 . (64 . 79))
    (458 . (1103 . 1110))
    ((381 . 385) . 930)
    (424 . (1018 . 1028))
    (548 . (265 . 266))
    (349 . (1156 . 1160))
    ((354 . 374) . 1156)
    ((410 . 419) . 470)
    ((407 . 413) . 701)
    ((469 . 479) . 528)
    (545 . (292 . 312))
    ((465 . 481) . 912)
    ((371 . 381) . 339)
    (525 . (728 . 747))
    (385 . (1452 . 1456))
    (543 . (781 . 806))
    ((376 . 399) . 426)
    (418 . (38 . 59))
    (451 . (461 . 472))
    (512 . (223 . 235))
    (449 . (174 . 179))
    (447 . (755 . 757))
    ((341 . 363) . 991)
    (394 . (1170 . 1190))
    (406 . (1429 . 1439))
    (370 . (1522 . 1534))
    (511 . (860 . 880))
    (381 . (919 . 930))
    (402 . (243 . 264))
    ((504 . 516) . 827)
    ((336 . 360) . 711)
    ((447 . 465) . 300)
    (434 . (614 . 616))
    (356 . (807 . 818))
    ((462 . 483) . 343)
    ((345 . 365) . 482)
    (410 . (796 . 816))
    (363 . (813 . 815))
    (406 . (1553 . 1565))
    (405 . (1507 . 1518))
    ((539 . 545) . 312)
    (462 . (729 . 738))
    ((484 . 500) . 1130)
    ((510 . 531) . 1043)
    ((382 . 395) . 766)
    (389 . (1209 . 1212))
    (352 . (823 . 829))
    (432 . (528 . 530))
    (337 . (1068 . 1072))
    ((453 . 455) . 322)
    ((365 . 391) . 780)
    (538 . (780 . 806))
    (400 . (858 . 867))
    (459 . (470 . 476))
    (469 . (649 . 663))
    (343 . (1367 . 1377))
    (456 . (1448 . 1460))
    (518 . (532 . 556))
    (356 . (994 . 1018))
    ((352 . 361) . 1559)
    (367 . (31 . 47))
    (350 . (1352 . 1364))
    ((353 . 359) . 840)
    ((384 . 412) . 1113)
    (461 . (436 . 447))
    (351 . (560 . 575))
    ((399 . 402) . 393)
    ((385 . 389) . 1372)
    ((531 . 538) . 508)
    ((407 . 413) . 698)
    ((503 . 512) . 999)
    (450 . (1234 . 1244))
    ((348 . 372) . 1347)
    (382 . (1548 . 1554))
    (550 . (1436 . 1448))
    ((407 . 409) . 983)
    (437 . (882 . 889))
    ((534 . 541) . 117)
    (459 . (663 . 664))
    (543 . (1298 . 1309))
    ((431 . 434) . 1371)
    (383 . (22 . 26))
    ((381 . 385) . 1293)
    (508 . (1086 . 1095))
    ((420 . 423) . 235)
    ((533 . 548) . 266)
    (389 . (1361 . 1372))
    (403 . (901 . 904))
    (520 . (760 . 785))
    ((361 . 377) . 1046)
    (433 . (1191 . 1201))
    ((539 . 544) . 1162)
    ((399 . 479) . 620)
    ((364 . 370) . 966)
    (446 . (1291 . 1302))
    (431 . (839 . 841))
    (530 . (839 . 852))
    (383 . (626 . 643))
    (447 . (233 . 255))
    (536 . (1595 . 1616))
    (549 . (991 . 1008))
    (354 . (596 . 608))
    (480 . (470 . 474))
    (395 . (1406 . 1418))
    (463 . (815 . 837))
    (510 . (11 . 20))
    ((480 . 506) . 1502)
    (478 . (1209 . 1216))
    (436 . (1252 . 1263))
    (409 . (972 . 983))
    ((348 . 361) . 1260)
    ((513 . 516) . 361)
    (408 . (65 . 89))
    (503 . (1408 . 1430))
    (444 . (1428 . 1441))
    (410 . (1507 . 1518))
    (532 . (659 . 678))
    (508 . (1407 . 1430))
    ((340 . 348) . 537)
    (341 . (1540 . 1565))
    (377 . (1481 . 1490))
    (342 . (455 . 476))
    (340 . (561 . 575))
    ((504 . 527) . 365)
    ((464 . 479) . 1011)
    (348 . (397 . 399))
    (424 . (614 . 616))
    (420 . (839 . 841))
    ((433 . 436) . 922)
    (524 . (1087 . 1093))
    (411 . (338 . 345))
    (374 . (1133 . 1156))
    (335 . (1468 . 1473))
    (402 . (295 . 314))
    ((395 . 417) . 1418)
    ((429 . 453) . 744)
    (534 . (95 . 117))
    (479 . (611 . 620))
    ((398 . 410) . 1208)
    ((373 . 375) . 636)
    (373 . (921 . 935))
    ((457 . 464) . 356)
    ((500 . 505) . 893)
    (372 . (64 . 69))
    ((423 . 435) . 54)
    (384 . (150 . 176))
    (464 . (993 . 1011))
    ((337 . 344) . 633)
    ((389 . 393) . 1336)
    (410 . (1383 . 1395))
    (372 . (1341 . 1347))
    (463 . (955 . 958))
    ((405 . 414) . 499)
    (483 . (999 . 1015))
    (346 . (758 . 760))
    (500 . (1285 . 1289))
    ((453 . 456) . 718)
    ((390 . 409) . 167)
    (373 . (104 . 113))
    ((374 . 380) . 199)
    (418 . (1237 . 1240))
    (453 . (735 . 744))
    (469 . (527 . 528))
    (478 . (159 . 168))
    (426 . (804 . 820))
    (461 . (1176 . 1179))
    ((451 . 470) . 883)
    ((538 . 545) . 451)
    (370 . (713 . 719))
    (392 . (899 . 912))
    (392 . (252 . 277))
    (522 . (1356 . 1376))
    (354 . (1132 . 1156))
    (340 . (519 . 537))
    (367 . (813 . 815))
    ((354 . 362) . 356)
    ((387 . 408) . 89)
    ((437 . 443) . 1353)
    (403 . (1021 . 1023))
    ((367 . 370) . 1041)
    (367 . (1249 . 1266))
    (522 . (1059 . 1079))
    (464 . (471 . 476))
    (423 . (625 . 636))
    (400 . (105 . 113))
    ((471 . 497) . 362)
    (492 . (1550 . 1552))
    (531 . (1474 . 1484))
    (383 . (204 . 230))
    (531 . (503 . 508))
    (392 . (922 . 935))
    (435 . (658 . 661))
    ((369 . 393) . 1459)
    ((469 . 485) . 386)
    (391 . (204 . 230))
    (437 . (416 . 427))
    (364 . (1068 . 1072))
    ((427 . 431) . 789)
    ((412 . 414) . 1067)
    (524 . (1260 . 1273))
    (414 . (476 . 499))
    (521 . (227 . 232))
    ((339 . 341) . 1617)
    ((432 . 439) . 393)
    ((540 . 548) . 540)
    (342 . (1240 . 1246))
    (473 . (731 . 733))
    (385 . (919 . 930))
    (504 . (205 . 208))
    (445 . (340 . 343))
    ((356 . 368) . 671)
    (520 . (52 . 62))
    ((432 . 447) . 31)
    (407 . (990 . 1002))
    ((402 . 410) . 816)
    (471 . (708 . 717))
    (544 . (424 . 426))
    (513 . (359 . 361))
    (404 . (1380 . 1398))
    (339 . (408 . 426))
    ((519 . 532) . 755)
    (398 . (171 . 193))
    (430 . (817 . 827))
    (395 . (37 . 59))
    ((453 . 458) . 589)
    (452 . (1294 . 1307))
    ((366 . 384) . 796)
    (373 . (299 . 309))
    ((341 . 344) . 660)
    (511 . (205 . 208))
    ((538 . 540) . 1223)
    (429 . (1214 . 1231))
    (479 . (25 . 42))
    (482 . (799 . 818))
    (455 . (269 . 278))
    (385 . (298 . 320))
    (532 . (1260 . 1273))
    (361 . (1510 . 1530))
    (470 . (752 . 764))
    (429 . (1270 . 1280))
    (441 . (441 . 453))
    (356 . (614 . 633))
    (353 . (1520 . 1527))
    ((339 . 361) . 426)
    (373 . (1350 . 1378))
    ((375 . 377) . 1490)
    (470 . (437 . 447))
    (443 . (420 . 430))
    (396 . (84 . 86))
    ((504 . 511) . 208)
    (545 . (1514 . 1532))
    (429 . (1449 . 1460))
    (454 . (1491 . 1495))
    ((496 . 520) . 275)
    (500 . (887 . 893))
    (426 . (566 . 578))
    ((417 . 423) . 752)
    (415 . (1237 . 1240))
    (427 . (770 . 789))
    (518 . (29 . 37))
    ((484 . 486) . 165)
    (348 . (597 . 608))
    (463 . (1542 . 1545))
    ((420 . 431) . 839)
    (426 . (857 . 867))
    ((478 . 480) . 660)
    ((497 . 524) . 211)
    (372 . (214 . 241))
    (552 . (419 . 430))
    (522 . (52 . 62))
    ((406 . 411) . 1333)
    (434 . (1324 . 1327))
    (542 . (886 . 903))
    (477 . (307 . 319))
    (340 . (1438 . 1454))
    (361 . (1037 . 1046))
    ((471 . 492) . 717)
    ((332 . 352) . 829)
    ((380 . 406) . 1439)
    (384 . (1107 . 1113))
    (445 . (1363 . 1375))
    (431 . (769 . 789))
    (490 . (1093 . 1112))
    (527 . (349 . 365))
    (463 . (713 . 723))
    (340 . (1188 . 1195))
    (373 . (1596 . 1601))
    (390 . (493 . 515))
    (539 . (1426 . 1450))
    (420 . (658 . 661))
    ((448 . 452) . 781)
    ((346 . 361) . 122)
    (399 . (626 . 636))
    (511 . (1234 . 1250))
    (434 . (1368 . 1371))
    ((535 . 544) . 162)
    (380 . (186 . 199))
    (461 . (1343 . 1356))
    (374 . (187 . 199))
    ((414 . 438) . 599)
    (456 . (794 . 807))
    (469 . (1325 . 1337))
    (484 . (93 . 109))
    (418 . (7 . 24))
    (485 . (241 . 254))))
