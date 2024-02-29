"""Write a program that calculates your final percentage in CSSE1001.

Your final percentage is based on your results in MyPyTutor, 
the three assignments and the final exam.

Challenge 1: Output your grade based on your final percentage
Challenge 2: Output your grade based on the course rules
             See: https://course-profiles.uq.edu.au/student_section_loader/section_5/109483
"""

MPT = input("Please enter your MyPyTutor mark: ")
A1 = input("Please enter your mark for the first assignment: ")
A2 = input("Please enter your mark for the second assignment: ")
A3 = input("Please enter your mark for the third assignment: ")
exam = input("Please enter your mark for the final exam: ")

def calculate_final_percentage(MPT, A1, A2, A3, exam):
    # Weightings for different components
    MPT_weight = 0.1
    A1_weight = 0.1
    A2_weight = 0.15
    A3_weight = 0.2
    exam_weight = 0.45

    # Calculate the final percentage
    mark = MPT*MPT_weight + A1*A1_weight + A2*A1_weight + A3*A3_weight + exam*exam_weight
    return mark

def assign_grade_course_rules(mark, exam):
    # Assign a grade based on course rules
    if mark >= 85 and exam >=80:
        return 7
    elif mark >= 75 and exam >=70:
        return 6
    elif mark >= 65 and exam >= 60:
        return 5
    elif mark >= 50 and exam >= 45:
        return 4
    elif mark >= 45 and exam >= 40:
        return 3
    elif mark >= 20:
            return 2
    else:
        return 1
    

