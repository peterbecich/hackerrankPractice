import math

1 + 1

stocks = [100, 113, 110, 85, 105, 102, 86, 63, 81, 101, 94, 106, 101, 79, 94, 90, 97]
stockDiffs = [13,-3,-25,20,-3,-16,-23,18,20,-7,12,-5,-22,15,-4,7]

def findMaxCrossingSubarray(arr, low, mid, high):
    leftSum = -10000
    sum = 0
    maxLeft = mid
    for i in range(mid, low, -1):
        sum = sum + arr[i]
        if (sum > leftSum):
            leftSum = sum
            maxLeft = i
    rightSum = -10000
    sum = 0
    maxRight = mid+1
    for i in range(mid+1, high):
        sum = sum + arr[i]
        if (sum > rightSum):
            rightSum = sum
            maxRight = i
    return (maxLeft, maxRight, leftSum + rightSum)

def findMaxSubarray(arr, low, high):
    if (low == high):
        return (low, high, arr[low])
    else:
        mid = math.floor((low+high)/2)
        (leftLow, leftHigh, leftSum) = findMaxSubarray(arr, low, mid)
        (rightLow, rightHigh, rightSum) = findMaxSubarray(arr, mid+1, high)
        (crossLow, crossHigh, crossSum) = findMaxCrossingSubarray(arr, low, mid, high)

        if(leftSum >= rightSum and leftSum >= crossSum):
            return (leftLow, leftHigh, leftSum)
        elif (rightSum >= leftSum and rightSum >= crossSum):
            return (rightLow, rightHigh, rightSum)
        else:
            return (crossLow, crossHigh, crossSum)
    
        

bestStockInterval = findMaxSubarray(stockDiffs, 0, len(stockDiffs)-1)
