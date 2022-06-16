package mobiliquest

import io.minio._
import better.files._
import io.minio.errors.MinioException
import io.minio.http.Method
import io.minio.messages.Item

import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.{Failure, Success, Try}


object FileService {

  private lazy val minioClient = MinioClient.builder
    .endpoint(Settings.endpoint)
    .credentials("mathieu", "mathieu-password")
    .build()

  val presenceUtileName = "bd-presence-utile"


  def withPresenceUtile[T](op: File => T) =
    withTmpDir[T] { file =>
      downloadBucketContent(presenceUtileName, file)
      op(file)
    }

  def withTmpDir[T](f: File ⇒ T) = {
    val file = java.nio.file.Files.createTempDirectory("MBQ").toFile.getAbsolutePath.toFile
    try {
      f(file)
    }
    finally {
      file.delete()
    }
  }

  def getURL(bucketName: String, fileName: String) = {
    minioClient.getPresignedObjectUrl(
      GetPresignedObjectUrlArgs.builder()
        .method(Method.GET)
        .bucket(bucketName)
        .`object`(fileName)
        .build
    )

  }

  def exists(bucketName: String) = {
    minioClient.bucketExists(BucketExistsArgs.builder.bucket(bucketName).build)
  }

  def createBucket(name: String) = {
    if (!exists(name)) {
      minioClient.makeBucket(MakeBucketArgs.builder.bucket(name).build)
    }
  }

  type TryResult[T] = Either[T, String]

  def tryTo[T](op: () => T): TryResult[T] = {
    Try(op()) match {
      case Success(s) => Left(s)
      case Failure(ex) => ex match {
        case (e: MinioException) =>
          val text = e.getMessage + "\n" + e.httpTrace
          println("[MINI IO ERROR] " + text)
          Right(text)
        case (e: Throwable) =>
          val text = e.getMessage + "\n" + e.getStackTrace.mkString("\n")
          println("[ERROR] " + text)
          Right(text)
      }
    }
  }

  def bucketContent(name: String): Iterator[Result[Item]] = {
    if (exists(name)) {
      minioClient.listObjects(ListObjectsArgs.builder.recursive(true).bucket(name).build).iterator().asScala
    } else Iterator[Result[Item]]()
  }

  def createDirectory(bucket: String, directoryName: String) = {
    (println("Create " + directoryName))
    minioClient.listObjects(
      ListObjectsArgs.builder
        .bucket(bucket)
        .prefix(directoryName)
        .build())
  }

  def uploadFiles(files: Seq[File], skipPath: String, bucket: String) = {
    createBucket(bucket)

    files.foreach { f =>
      val targetPath = f.pathAsString.replace(skipPath, "")
      if (f.isDirectory) createDirectory(bucket, targetPath)
      else {
        minioClient.uploadObject(
          UploadObjectArgs.builder
            .bucket(bucket)
            .`object`(targetPath)
            .filename(f.pathAsString).build)
      }
    }
  }


  def downloadBucketContent(bucketName: String, target: File) = {
    tryTo(() =>
      bucketContent(bucketName).foreach { result =>
        val objectName = result.get.objectName
        val fullTarget = target / objectName.split("/").dropRight(1).mkString("/")
        if (!fullTarget.exists) fullTarget.createDirectories
        minioClient.downloadObject(
          DownloadObjectArgs.builder
            .bucket(bucketName)
            .`object`(result.get.objectName)
            .filename((target / objectName).pathAsString)
            .overwrite(true)
            .build
        )
      }
    )
  }


}
