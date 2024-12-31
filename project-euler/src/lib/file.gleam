import file_streams/file_stream
import file_streams/file_stream_error.{type FileStreamError}
import gleam/bit_array
import gleam/result

pub type ReadFileError {
  FileError(error: FileStreamError)
  DecodeError
}

pub fn read(path: String) {
  use stream <- result.try(
    file_stream.open_read(path) |> result.map_error(FileError),
  )

  use data <- result.try(
    file_stream.read_remaining_bytes(stream) |> result.map_error(FileError),
  )
  use data <- result.try(
    bit_array.to_string(data) |> result.replace_error(DecodeError),
  )

  Ok(data)
}
